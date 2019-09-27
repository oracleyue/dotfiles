#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# from the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --exclude=.DS_Store --delete'
repopath=$HOME'/Workspace/gitrepo/dotfiles'

# push updates to github.com
cd $repopath && git pull origin master && cd

# rsync .emacs or init.el
$sync $repopath/_emacs.d/init.el ~/.emacs.d/init.el
# sed 's/Sans Mono-[0-9][0-9]/Sans Mono-15/' \
#     $repopath/_emacs.d/init.el > ~/.emacs.d/init.el

# rsync .emacs.d (essential packages)
$sync $repopath/_emacs.d/init ~/.emacs.d/
$sync --exclude-from="$HOME/.emacs.d/git/_exclude-list" \
      $repopath/_emacs.d/git ~/.emacs.d/
$sync --exclude="github" \
      $repopath/_emacs.d/themes ~/.emacs.d/
$sync $repopath/_emacs.d/snippets ~/.emacs.d/
$sync $repopath/_emacs.d/templates ~/.emacs.d/
