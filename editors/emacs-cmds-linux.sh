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


# =======================================================
# Function: list/start/stop emacs severs
# =======================================================

function es() {
    if [[ $# -eq 0 ]] || [[ "$1" == "list" ]]; then
        ps aux | grep emacs

    elif [[ "$1" == "stop" ]]; then
        /usr/bin/emacsclient --eval "(kill-emacs)"

    elif [[ "$1" == "start" ]]; then
        /usr/bin/emacs --daemon
        # if not set LC_CTYPE globally, use:
        # LC_CTYPE=zh_CN.UTF-8 /usr/bin/emacs --daemon

    else
        echo 'Usages: '
        echo '  - es (list): check server list;'
        echo '  - es stop  : kill the emacs server;'
        echo '  - es start : start the emacs server.'
    fi
}

# =======================================================
# Function: emacs client
# =======================================================

# emacsclient: main
function ec() {
    /usr/bin/emacsclient -nc $@
}
