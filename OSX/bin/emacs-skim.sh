#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is for Skim to raise Emacs in backward search. It does the
# following:
# - test if a frame of the Emacs server "main" has been created;
# - if not, create one; otherwise, connect the available frame.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 31 May 2018

# run server first by
#     ~$ emacs --daemon=main

status=$(/usr/local/bin/emacsclient --no-wait --socket-name=main \
                                    --eval "(display-graphic-p)")

if [ $status = "nil" ]; then
    /usr/local/bin/emacsclient --socket-name=main -c "$@"
else
    /usr/local/bin/emacsclient --socket-name=main "$@"
fi
