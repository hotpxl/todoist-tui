# Todoist TUI

Text user interface for Todoist.

The native Todoist client is already good enough. But I wanted something to run in terminal for greater efficiency. I created this project also to try out some Haskell libraries.

## Usage

Put your Todoist API token under `~/.config/todoist`.

Current supported functionalities include:
* Add item
* Modify item
* Complete item
* Reorder

I will try to support due dates if I somehow need to use them. Currently everything goes into Inbox project and stays there. This is my workflow. So I am not going to support multiple projects.

## Keys

| Key              | Action                   |
|------------------|--------------------------|
| <kbd>j</kbd>     | Move cursor down         |
| <kbd>k</kbd>     | Move cursor up           |
| <kbd>c</kbd>     | Start creating new item  |
| <kbd>q</kbd>     | Commit and quit          |
| <kbd>=</kbd>     | Move selection up        |
| <kbd>-</kbd>     | Move selection down      |
| <kbd>Enter</kbd> | Finish creating new item |
| <kbd>Space</kbd> | Mark as complete         |
