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
        systemctl status --user emacs

    elif [[ "$1" == "stop" ]]; then
        systemctl stop --user emacs

    elif [[ "$1" == "restart" ]]; then
        systemctl restart --user emacs

    else
        echo 'Usages: '
        echo '  - es: check server status;'
        echo '  - es stop: kill the emacs server'
        echo '  - es restart: restart the emacs server'
    fi
}

# =======================================================
# Function: emacs client
# =======================================================

# emacsclient: main
function ec() {
    /usr/bin/emacsclient -nc $@
}
