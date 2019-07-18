#!/bin/bash
# -*- coding: utf-8 -*-
#
# Batchly rename files with a prefix and order numbers.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 18 Jul 2019


prefix="archlinux-wallpaper-"
num=0

for file in $(find . -type f \( -iname \*.jpg -o -iname \*.png \)); do
    ext=${file##*.}
    ((num++))
    mv $file $prefix$num.$ext
done
