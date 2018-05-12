#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.vimrc" and essentials of "~/.vim/"
# to the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store --exclude=.git'
repopath=$HOME'/Workspace/gitrepo/dotfiles/editors'

# syncronize
$sync ~/.vimrc $repopath/_vimrc
$sync ~/.vim/ $repopath/_vim

# push updates to github.com
cd $repopath
git add -A
git commit -m "update vim config from mac"
git push
cd ~