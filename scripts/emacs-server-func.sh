#!/bin/bash
# -*- coding: utf-8 -*-
#
# Functions for emacs servers: start, stop, and list under running.

# Copyright (c) 2014-2017, Zuogong YUE
# Author: Zuogong YUE
#         https://github.com/oracleyue
# Licensed under the GNU General Public License
#
# Last modified on 03 Sep 2017



# =======================================================
# Function: list/start/stop emacs severs
# =======================================================

function es() {
    tmpfile="$HOME/.tmp.stdout"
    if [[ $(uname) == "Linux" ]]; then
        EMACS="/usr/bin/emacs"
    else
        EMACS="/usr/local/bin/emacs"
    fi

    if [[ $# -eq 0 ]] || [[ "$1" == "list" ]]; then
        ps aux | grep -i 'emacs --daemon' | grep -v 'grep' \
            | awk '{print $2 "\t" $9 "\temacs " $12}' > $tmpfile
        while read line; do
            echo "$line" | sed 's/\0123,4\012//p'
        done < $tmpfile
        rm -f $tmpfile

    elif [[ "$1" == "stop" ]]; then
        if [[ -z $2 ]]; then
	        kill $(ps aux | grep -i 'emacs --daemon' | grep -v 'grep' | awk '{print $2}')
        else
            case $2 in
                m)
                    kill $(ps aux | grep -i 'emacs --daemon' | grep "main" | grep -v 'grep' | awk '{print $2}')
                    ;;
                c)
                    kill $(ps aux | grep -i 'emacs --daemon' | grep "coding" | grep -v 'grep' | awk '{print $2}')
                    ;;
                a)
                    kill $(ps aux | grep -i 'emacs --daemon' | grep "ac-mode" | grep -v 'grep' | awk '{print $2}')
                    ;;
                *)
                    kill $(ps aux | grep -i 'emacs --daemon' | grep "$1" | grep -v 'grep' | awk '{print $2}')
                    ;;
            esac
        fi

    elif [[ "$1" == "start" ]]; then
        if [[ -z $2 ]]; then
            $EMACS --daemon=main
            $EMACS --daemon=coding
        else
            case $2 in
                m)
                    $EMACS --daemon=main
                    ;;
                c)
                    $EMACS --daemon=coding
                    ;;
                a)
                    $EMACS --daemon=ac-mode
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
# Function: emacs client (main & coding)
# =======================================================

# emacsclient: main
function ec() {
    if [[ $(uname) == "Linux" ]]; then
        EMACSCLIENT="/usr/bin/emacsclient"
    else
        EMACSCLIENT="/usr/local/bin/emacsclient"
    fi

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nc --server-file=main
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nc --server-file=main $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "main" server;'
        echo '  - 1: run emacsclient to open FILES you specified.'
    fi
}

# emacsclient: coding
function ecc() {
    if [[ $(uname) == "Linux" ]]; then
        EMACSCLIENT="/usr/bin/emacsclient"
    else
        EMACSCLIENT="/usr/local/bin/emacsclient"
    fi

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nc --server-file=coding
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nc --server-file=coding $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "coding" server;'
        echo '  - 1: run emacsclient to open FILES you specified.'
    fi
}
