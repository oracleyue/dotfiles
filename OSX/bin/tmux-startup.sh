#!/bin/bash
# last modified 03 Jan 2025

if [[ -z $TMUX ]] && [[ -z $(tmux ls 2>&1 | grep "local") ]]; then
    # local session
    tmux new-session -d -s local -n main
    tmux new-window -t local -n files
    tmux new-window -t local -n workspace
    tmux split-window -h -t local:3
    tmux select-window -t local:1

    # ssh session
    # tmux new-session -d -s ssh -n main
    # tmux new-window -t ssh -n monitor

    exit
fi
