#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script to do bulk convertion from *.flv to *.mp3 via avconv .

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 26 Mar 2018


FPATH=$1
FILES=$(find $FPATH -name "*.flv")

echo "List of files to convert:"
find $FPATH -name "*.flv" -exec basename {} \;
echo

read -p "Start avconv? [Y/n] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    IFS=$'\n'
    for FILE in $FILES; do
        # echo $FILE
        avconv -i "$FILE" "${FILE%.flv}.mp3"
    done
    echo "Finished!"
else
    # handle exits from shell or function but don't exit interactive shell
    [[ "$0" = "$BASH_SOURCE" ]] && exit 1 || return 1
fi
