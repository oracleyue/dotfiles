#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to randomly rename files in batch.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 11 Apr 2019


for file in *.mp4; do
    nfile=$(date +%s | shasum | head -c 20)
    sleep 1
    echo $file
    printf "%s\n" $nfile
    mv "$file" $nfile
    echo
done
