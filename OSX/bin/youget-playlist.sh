#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to use you-get to donwload part of items from a playlist.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 29 Jan 2022


# arguments
numbers=$1
url=$2

# parsing indexes
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Example: you-get_items.sh 14-28 [URL]"
    exit 0
else
    if [[ "$1" == *"-"* ]]; then
        start=${numbers%-*}
        end=${numbers#*-}
        items=$(seq $start 1 $end)
    elif [[ "$1" == *","* ]]; then
        i=1
        while [[ $numbers != "" ]]; do
            items[$i]=$(echo $numbers | cut -d, -f1)
            (( i=i+1 ))
            numbers=$(echo $numbers | cut -s -d, -f2-)
        done
    fi
fi

# downloading
for item in ${items[*]}; do
    item_url="$url?p=$item"

    echo "[URL]: $item_url"
    echo "[START] >>>"
    you-get $item_url | tee -a you-get.log
    echo "[END]"; echo; echo
done
