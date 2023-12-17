#!/bin/bash
# -*- coding: utf-8 -*-
#
# Clean up empty folders (except hidden meta files) in Zotero storage.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 04 Feb 2023


ztFolder="$HOME/Papers/Zotero/storage"

for item in $(ls $ztFolder)
do
    folder="$ztFolder/$item"
    if [ -z "$(ls $folder)" ]; then
        rm -rf $folder
    fi
done
