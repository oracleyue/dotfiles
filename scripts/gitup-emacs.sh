#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# to the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/dotfiles'

# rsync .emacs and .emacs.d to the local git repo
# $sync ~/.emacs $repopath/_emacs.25.1.osx
$sync ~/.emacs.d/init $repopath/_emacs.d.25.1.osx/
$sync --exclude="clang-complete" --exclude="*.pyc" \
      ~/.emacs.d/git $repopath/_emacs.d.25.1.osx/
$sync ~/.emacs.d/themes $repopath/_emacs.d.25.1.osx/
$sync ~/.emacs.d/snippets $repopath/_emacs.d.25.1.osx/
$sync ~/.emacs.d/default-css $repopath/_emacs.d.25.1.osx/

# push updates to github.com
cd $repopath
git add -A
git commit -m "update emacs config from mac"
git push
cd ~