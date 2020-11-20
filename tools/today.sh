#!/usr/bin/env bash
#
# Open VSCode with today's daily note. If VSCode is already, this script will
# bring it to front with the daily note opened.
#
set -xe

NOTEBOOK=zk

code --folder-uri=$HOME/Documents/$NOTEBOOK $HOME/Documents/$NOTEBOOK/$(date +%Y-%m-%d.md) 
xdotool windowactivate `xdotool search --name --limit 1 "$NOTEBOOK - Visual Studio Code"`
