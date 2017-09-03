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


function es() {
    tmpfile="$HOME/.tmp.stdout"

    if [[ $# -eq 0 ]] || [[ "$1" == "list" ]]; then
        ps aux | grep -i 'emacs --daemon' | grep -v 'grep' \
            | awk '{print $2 "\t" $9 "\tEmacs " $12}' > $tmpfile
        while read line; do
            echo "$line" | sed -n 's/\0123,4\012//p'
        done < $tmpfile
        rm $tmpfile

    elif [[ "$1" == "stop" ]]; then
        kill $(ps aux | grep -i 'Emacs --daemon' | grep -v 'grep' | awk '{print $2}')

    elif [[ "$1" == "start" ]]; then
        if [[ -z $2 ]]; then
            $HOME/bin/emacs --daemon=main
        else
            $HOME/bin/emacs --daemon="$2"
        fi

    else
        echo 'usage: 0, 1 or 2 arguments'
        echo '  - 0: list running servers;'
        echo '  - 1: choose among "list, start, stop"; "start" use "main" as server name;'
        echo '  - 2: only for "es start SERVER_NAME" as you specified.'
    fi
}

function ec() {
    if [[ $# -eq 0 ]]; then
        $HOME/bin/emacsclient -nc --server-file=main
    elif [[ -n $1 ]]; then
        $HOME/bin/emacsclient -nc --server-file=main $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "main" server;'
        echo '  - 1: run emacsclient to open FILES you specified.'
    fi
}
