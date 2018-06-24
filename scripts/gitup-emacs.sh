#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# to the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/dotfiles'

# rsync .emacs OR init.el
# $sync ~/.emacs $repopath/_emacs.25.1.osx
$sync ~/.emacs.d/init.el $repopath/_emacs.d/init.el

# rsync .emacs.d (essential packages)
$sync ~/.emacs.d/init $repopath/_emacs.d/
$sync --exclude="clang-complete" --exclude="*.pyc" \
      ~/.emacs.d/git $repopath/_emacs.d/
$sync --exclude="github" \
      ~/.emacs.d/themes $repopath/_emacs.d/
$sync ~/.emacs.d/snippets $repopath/_emacs.d/
$sync ~/.emacs.d/templates $repopath/_emacs.d/

# rsync important scripts
$sync ~/bin/gitup-emacs.sh $repopath/scripts/
$sync ~/bin/gitpull-emacs.sh $repopath/scripts/
$sync ~/bin/emacs-server-func.sh $repopath/scripts/

# push updates to github.com
cd $repopath
git add -A
git commit -m "update emacs config from mac"
git push
cd ~