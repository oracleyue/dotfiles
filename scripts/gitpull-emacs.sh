#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# from the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/dotfiles'

# push updates to github.com
cd $repopath && git pull && cd ~

# rsync .emacs and .emacs.d from github repo
sed 's/Sans Mono-[0-9][0-9]/Sans Mono-15/' $repopath/_emacs.25.1.osx > ~/.emacs

$sync $repopath/_emacs.d.25.1.osx/init ~/.emacs.d/
$sync --exclude="clang-complete" --exclude="*.pyc" \
      $repopath/_emacs.d.25.1.osx/git ~/.emacs.d/
$sync $repopath/_emacs.d.25.1.osx/themes ~/.emacs.d/
$sync $repopath/_emacs.d.25.1.osx/snippets ~/.emacs.d/
$sync $repopath/_emacs.d.25.1.osx/templates ~/.emacs.d/
