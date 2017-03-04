{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Data.List
import Data.Maybe
import System.IO

import Brick.Widgets.Core ((<+>))
import Data.Aeson ((.:))
import Control.Lens ((&), (.~), (^.))

import qualified Brick as B
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Core as BWC
import qualified Brick.Widgets.Center as BWCenter
import qualified Brick.Widgets.List as BWL
import qualified Brick.Widgets.Edit as BWE
import qualified Brick.Widgets.Border as BWB
import qualified Brick.AttrMap as BA
import qualified Graphics.Vty as GV
import qualified Data.Vector as DV
import qualified Brick.Focus as BF
import qualified Data.Text.Zipper as DTZ
import qualified Network.Wreq as NW
import qualified Data.Text as DT
import qualified Data.UUID.V4 as DUV4
import qualified Data.UUID as DU
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Aeson as DA
import qualified System.Directory as SD
import qualified System.FilePath.Posix as SFP

data TodoistState = TodoistState
  { items :: [Item]
  , currentIndex :: Int
  , focusRing :: BF.FocusRing Name
  , editor :: BWE.Editor String Name
  }

data Name
  = ListView
  | Editor
  deriving (Ord, Eq, Show)

listDrawElement :: Bool -> String -> B.Widget Name
listDrawElement _ = BWC.padRight BT.Max . BWC.str

drawUI :: TodoistState -> [BT.Widget Name]
drawUI state = [ui]
  where
    TodoistState { items = items
                 , currentIndex = currentIndex
                 , focusRing = focusRing
                 , editor = editor
                 } = state
    ed = BF.withFocusRing focusRing BWE.renderEditor editor
    l =
      BWL.list
           ListView
           (DV.fromList
              (map
                 (\Item {content = content, itemState = itemState} ->
                    (case itemState of
                       New -> "[+] "
                       Cancel -> "[X] "
                       Delete _ -> "[X] "
                       Existing _ -> "[ ] ") ++
                    content)
                 items))
           1 &
         BWL.listMoveTo currentIndex
    ui =
      BWC.vBox
        [ BWC.str "Inbox" & BWB.borderWithLabel $
          BWL.renderList listDrawElement True l
        , BWC.str "> " <+> BWC.vLimit 1 ed
        ]

appEvent :: TodoistState
         -> BT.BrickEvent Name e
         -> BT.EventM Name (BT.Next TodoistState)
appEvent l (BT.VtyEvent e) =
  let TodoistState { items = items
                   , currentIndex = currentIndex
                   , focusRing = focusRing
                   , editor = editor
                   } = l
  in case BF.focusGetCurrent focusRing of
       Nothing -> BM.continue l
       Just ListView ->
         case e of
           GV.EvKey GV.KEsc [] -> BM.halt l
           GV.EvKey (GV.KChar ' ') [] ->
             BM.continue
               TodoistState
               { items = markItem currentIndex items
               , currentIndex = currentIndex
               , focusRing = focusRing
               , editor = editor
               }
           GV.EvKey (GV.KChar 'q') [] -> BM.halt l
           GV.EvKey (GV.KChar 'k') [] ->
             let newState =
                   TodoistState
                   { items = items
                   , currentIndex =
                       if 0 < currentIndex
                         then currentIndex - 1
                         else currentIndex
                   , focusRing = focusRing
                   , editor = editor
                   }
             in BM.continue newState
           GV.EvKey (GV.KChar 'j') [] ->
             let TodoistState { items = items
                              , currentIndex = currentIndex
                              } = l
             in let newState =
                      TodoistState
                      { items = items
                      , currentIndex =
                          if currentIndex < (length items) - 1
                            then currentIndex + 1
                            else currentIndex
                      , focusRing = focusRing
                      , editor = editor
                      }
                in BM.continue newState
           GV.EvKey (GV.KChar 'c') [] ->
             TodoistState
             { items = items
             , currentIndex = currentIndex
             , focusRing = BF.focusNext focusRing
             , editor = editor
             } &
             BM.continue
           _ -> BM.continue l
       Just Editor ->
         case e of
           GV.EvKey GV.KEnter [] ->
             let newContent = BWE.getEditContents editor & intercalate " "
             in TodoistState
                { items =
                    if newContent /= ""
                      then addItem newContent items & sort
                      else items
                , currentIndex = currentIndex
                , focusRing = BF.focusNext focusRing
                , editor = BWE.applyEdit DTZ.clearZipper editor
                } &
                BM.continue
           ev -> do
             nextEditor <- BWE.handleEditorEvent ev editor
             BM.continue $
               TodoistState
               { items = items
               , currentIndex = currentIndex
               , focusRing = focusRing
               , editor = nextEditor
               }
appEvent l _ = BM.continue l

data ResourceType = Projects | Items

resourceTypeToString :: ResourceType -> String
resourceTypeToString Projects = "projects"
resourceTypeToString Items = "items"

getResource :: String -> ResourceType -> IO DBL.ByteString
getResource token resourceType = do
  let url = "https://todoist.com/API/v7/sync"
  let opts =
        NW.defaults & NW.param "sync_token" .~ ["*"] & NW.param "resource_types" .~
        [DT.pack $ "[\"" ++ resourceTypeToString resourceType ++ "\"]"] &
        NW.param "token" .~
        [DT.pack token]
  r <- NW.getWith opts url
  return $ r ^. NW.responseBody

data ProjectList =
  ProjectList [Project] deriving (Show)

data Project = Project
  { name :: String
  , inbox :: Bool
  , id :: Int
  } deriving (Show)

instance DA.FromJSON Project where
  parseJSON =
    DA.withObject "Project" $ \o ->
      Project <$> o .: "name" <*> o .: "inbox_project" <*> o .: "id"

instance DA.FromJSON ProjectList where
  parseJSON =
    DA.withObject "ProjectList" $ \o ->
      (o .: "projects") >>= ((fmap ProjectList) . DA.parseJSON)

data ItemList =
  ItemList [Item]
  deriving (Show)

data ItemState
  = New
  | Cancel
  | Delete Int
  | Existing Int
  deriving (Show)

data Item = Item
  { content :: String
  , itemOrder :: Int
  , itemState :: ItemState
  } deriving (Show)

instance Eq Item where
  (==) a b = (itemOrder a) == (itemOrder b)

instance Ord Item where
  compare a b = compare (itemOrder a) (itemOrder b)

instance DA.FromJSON ItemList where
  parseJSON =
    DA.withObject "ItemList" $ \o ->
      (o .: "items") >>= fmap ItemList . DA.parseJSON

instance DA.FromJSON Item where
  parseJSON =
    DA.withObject "Item" $ \o ->
      Item <$> o .: "content" <*> o .: "item_order" <*>
      (o .: "id" >>= return . Existing)

addItem :: String -> [Item] -> [Item]
addItem content l =
  let len = length l
  in l ++ [Item {content = content, itemOrder = len + 1, itemState = New}]

markItem :: Int -> [Item] -> [Item]
markItem 0 (head:tail) =
  let Item {content = content, itemOrder = itemOrder, itemState = itemState} = head
  in Item
     { content = content
     , itemOrder = itemOrder
     , itemState =
         case itemState of
           Delete i -> Existing i
           Existing i -> Delete i
           New -> Cancel
           Cancel -> New
     } :
     tail
markItem idx (head:tail) =
  let newTail = markItem (idx - 1) tail
  in head : newTail
markItem _ [] = []


getProjects :: String -> IO [Project]
getProjects token = do
  res <- getResource token Projects
  let decoded = DA.decode res :: Maybe ProjectList
  case decoded of
    Nothing -> return $ []
    Just (ProjectList a) -> return $ a

getItems :: String -> IO [Item]
getItems token = do
  res <- getResource token Items
  let decoded = DA.decode res :: Maybe ItemList
  case decoded of
    Nothing -> return $ []
    Just (ItemList a) -> return $ a

commitItems :: String -> [Item] -> IO ()
commitItems token items = do
  let url = "https://todoist.com/API/v7/sync"
  strList <-
    foldl
      (\prev Item {content = content,  itemState = itemState} -> do
         prevStrList <- prev
         case itemState of
           New -> do
             uuid <- DUV4.nextRandom
             let newCommand =
                   "{\"type\":\"item_add\",\"temp_id\":\"\",\"uuid\":\"" ++
                   DU.toString uuid ++
                   "\",\"args\":{\"content\":\"" ++
                   content ++
                   "\"}}"
             return $ newCommand : prevStrList
           Delete id -> do
             uuid <- DUV4.nextRandom
             let newCommand =
                   "{\"type\":\"item_close\",\"temp_id\":\"\",\"uuid\":\"" ++
                   DU.toString uuid ++
                   "\",\"args\":{\"id\":" ++
                   show id ++
                   "}}"
             return $ newCommand : prevStrList
           _ -> return prevStrList)
      (return [])
      items
  if length strList == 0
    then return ()
    else do
      let commands = "[" ++ intercalate "," strList ++ "]"
      let opts =
            NW.defaults & NW.param "token" .~ [DT.pack token] &
            NW.param "commands" .~
            [DT.pack commands]
      res <- NW.getWith opts url
      if res ^. NW.responseStatus . NW.statusCode /= 200
        then putStrLn "An error occured."
        else return ()

main :: IO ()
main = do
  home <- SD.getHomeDirectory
  withFile (SFP.combine home ".config/todoist") ReadMode $ \handle -> do
    token <- hGetLine handle
    itemList <- getItems token
    let state =
          TodoistState
          { items = sort itemList
          , currentIndex = 0
          , focusRing = BF.focusRing [ListView, Editor]
          , editor = BWE.editor Editor (BWC.str . unlines) Nothing ""
          }
    let app =
          BM.App
          { appDraw = drawUI
          , appChooseCursor = BM.showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap =
              [(BWL.listSelectedAttr, BU.on GV.black GV.white)] &
              BA.attrMap GV.defAttr &
              const
          }
    finalState <- BM.defaultMain app state
    let TodoistState {items = items} = finalState
    commitItems token items
    return ()
