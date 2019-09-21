#!/bin/bash
# -*- coding: utf-8 -*-
#
# For Arch Linux with systemd support.
# Functions for emacs servers: start, stop, and list under running.

# Copyright (c) 2014-2017, Zuogong YUE
# Author: Zuogong YUE
#         https://github.com/oracleyue
# Licensed under the GNU General Public License
#
# Last modified on 03 May 2019


# =======================================================
# Preparations
# =======================================================

# 1. Copy the file "emacs.service" to the following:
#       $HOME/.config/systemd/user/
#    If the folder doesn't exist, create one.
# 2. Enable the service by:
#       systemctl enable --user emacs
#    If you want to see the effects immediately, run:
#       systemctl start --user emacs
#
# Okular PDF reverse Sync, if using "main" server:
#    emacsclient --socket-name=main --no-wait +%l %f


# =======================================================
# Function: list/start/stop emacs severs
# =======================================================

function es() {
    if [[ $# -eq 0 ]] || [[ "$1" == "list" ]]; then
        # ps aux | grep emacs
        ps aux | grep -i 'emacs [-bg]*-daemon' | grep -v 'grep' \
            | awk '{print $2 "\t" $9 "\temacs " $12}'

    elif [[ "$1" == "stop" ]]; then
        if [[ -z $2 ]]; then
        # /usr/bin/emacsclient --eval "(kill-emacs)"
	    kill -9 $(ps aux | grep -i 'emacs [-bg]*-daemon' \
                      | grep -v 'grep' | awk '{print $2}')
        else
            case $2 in
                m)
                    kill $(ps aux | grep -i 'emacs --daemon' \
                               | grep -v 'grep' | awk '{print $2}')
                    ;;
                c)
                    kill $(ps aux | grep -i 'emacs --bg-daemon' \
                               | grep "coding" | grep -v 'grep' | awk '{print $2}')
                    ;;
                *)
                    kill $(ps aux | grep -i 'emacs --bg-daemon' \
                               | grep "$2" | grep -v 'grep' | awk '{print $2}')
                    ;;
            esac
        fi

    elif [[ "$1" == "start" ]]; then
        if [[ -z $2 ]]; then
            #env LC_CTYPE=zh_CN.UTF-8 /usr/bin/emacs --daemon
            /usr/bin/emacs --daemon
            /usr/bin/emacs --bg-daemon=coding
        else
            case $2 in
                m)
                    /usr/bin/emacs --daemon
                    ;;
                c)
                    /usr/bin/emacs --bg-daemon=coding
                    ;;
                *)
                    $EMACS --daemon="$2"
                    ;;
            esac
        fi
    else
        echo 'usage: 0, 1 or 2 arguments'
        echo '  - 0: list running servers;'
        echo '  - 1: choose among "list, start, stop"; "start" use "main" as server name;'
        echo '  - 2: only for "es start SERVER_NAME" as you specified.'
    fi
}

# =======================================================
# Function: emacs client
# =======================================================

# emacsclient: main
function em() {
    /usr/bin/emacsclient -nc $@
}
# emacsclient: coding
function ec() {
    /usr/bin/emacsclient -nc --socket-name=coding $@
}
