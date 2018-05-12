#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and "~/.emacs.d/" to HomeTest

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/bin/HomeTest'

# rsync .emacs and .emacs.d to the local git repo
$sync ~/.emacs    $repopath/.emacs
$sync ~/.emacs.d/ $repopath/.emacs.d/
