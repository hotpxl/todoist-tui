# Todoist TUI

Text user interface for Todoist.

The native Todoist client is already good enough. But I wanted something to run in terminal for greater efficiency. I created this project also to try out some Haskell libraries.

## Usage

Put your Todoist API token under `~/.config/todoist`.

Current supported functionalities include:
* Add item.
* Complete item.

I will try to support due dates and modifying items if I somehow need to use them. Currently everything goes into Inbox project and stays there. This is my workflow. So I am not going to support multiple projects.

## Keys

* <kbd>j</kbd> Move selection down.
* <kbd>k</kbd> Move selection up.
* <kbd>c</kbd> Start creating new item.
* <kbd>q</kbd> Commit and quit.
* <kbd>Enter</kbd> Finish creating new item.
* <kbd>Space</kbd> Mark as complete.

