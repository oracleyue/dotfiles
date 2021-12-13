#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is for Skim to raise Emacs in backward search. It does the
# following:
# - test if a frame of the Emacs server "main" has been created;
# - if not, create one; otherwise, connect the available frame.
# When using client, run server first by
#   ~$ emacs --daemon=main
#
# Setup for Skim.app: "Preferences -> Sync: PDF-TEX Sync support"
# - "Preset": Custom
# - "Command": ~/bin/emacs-skim.sh
# - "Arguments": --no-wait +%line "%file"
#
# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 10 Jul 2018


status=$(/usr/local/bin/emacsclient --no-wait --socket-name=main \
                                    --eval "(display-graphic-p)")

if [ $status = "nil" ]; then
    /usr/local/bin/emacsclient --socket-name=main -c "$@"
else
    /usr/local/bin/emacsclient --socket-name=main "$@"
fi
