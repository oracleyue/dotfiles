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
if [[ $# -eq 0 ]]; then
    if [[ -f $LogFile ]]; then
        # remove progress bars
        gsed -i 's/[0-9 ]*\..*\/s//' $LogFile

        # extract title info
        gsed -n '/^title:.*/p' $LogFile > $TitleFile

        # clean title file
        gsed -i -e 's/^title: *//' $TitleFile
        gsed -i -e 's/$/\.mp4/' $TitleFile

        echo "Video titles extracting... [done]"
    else
        echo "No $LogFile found. Run 'you-get -lu URL > you-get.log' to retrieve meta information."
    fi
fi

# rename video files
if [[ "$1" == "--dry-run" || "$1" == "-d" ]]; then
    echo "List of videos to be renamed: [OLD >>> NEW]"
    echo "--------------------------------------"
    for file in *.mp4; do
        read line
        echo "$file    >>>    $line"
    done < $TitleFile
fi

# rename video files
if [[ "$1" == "--do" ]]; then
    # listing the old and new file names
    echo "List of videos to be renamed: [OLD >>> NEW]"
    echo "--------------------------------------"
    for file in *.mp4; do
        read line
        echo "$file    >>>    $line"
    done < $TitleFile

    # confirm to perform
    read -p "Are you sure? " -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        for file in *.mp4; do
            read line
            mv "$file" "$line"
        done < $TitleFile
        echo "video file renaming... [done]"
    else
        echo "[Suggestion] Try 'bilibili_rename.sh --dry-run' and edit your titles.txt"
    fi
fi
