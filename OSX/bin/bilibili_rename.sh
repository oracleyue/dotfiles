#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script uses the log file from `you-get -l URL | tee dl.log` to
# extract video titles and rename all videos.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 2021-10-12


# input files
LogFile='you-get.log'
TitleFile='titles.txt'

# extract video titles from metafile
if [ $# -eq 0 ]; then
    # remove progress bars
    gsed -i 's/[0-9 ]*\..*\/s//' $LogFile

    # extract title info
    gsed -n '/^title:.*/p' $LogFile > $TitleFile

    # clean title file
    gsed -i -e 's/^title: *//' $TitleFile
    gsed -i -e 's/$/\.mp4/' $TitleFile

    echo "video titles extracting... [done]"
fi

# rename video files
if [ "$1" == "do" ]; then
    for file in *.mp4; do
        read line
        mv "$file" "$line"
    done < $TitleFile

    echo "video file renaming... [done]"
fi
