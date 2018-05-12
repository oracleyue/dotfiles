#!/bin/bash
# -*- coding: utf-8 -*-
#
# This is to backup the hexo settings and blogs.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 24 Apr 2018


# Paths
srcpath="$HOME/Public/Dropbox/oracleyue/oracleyue.github.io"
despath="$HOME/Files/oracleyue/blog_backup"

# Commands
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'

# Operations
$sync $srcpath/_config.yml $despath/
$sync $srcpath/source $despath/
$sync $srcpath/themes $despath/
