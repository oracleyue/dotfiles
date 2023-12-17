#!/bin/bash
# -*- coding: utf-8 -*-
#
# To synchronize "~/Books" and "./Books".

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 13 Nov 2017


sync='rsync -rlptD --exclude=.DS_Store'

$sync --delete-excluded -P /Users/zyue/Books/  ./Books/
