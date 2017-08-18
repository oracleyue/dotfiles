#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# to the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'

# rsync .emacs and .emacs.d to the local git repo
$sync ~/.emacs ~/Workspace/gitrepo/dotfiles/_emacs.25.1.osx
$sync ~/.emacs.d/init ~/Workspace/gitrepo/dotfiles/_emacs.d.25.1.osx/
$sync ~/.emacs.d/git ~/Workspace/gitrepo/dotfiles/_emacs.d.25.1.osx/
$sync ~/.emacs.d/themes ~/Workspace/gitrepo/dotfiles/_emacs.d.25.1.osx/
$sync ~/.emacs.d/snippets ~/Workspace/gitrepo/dotfiles/_emacs.d.25.1.osx/
$sync ~/.emacs.d/default-css ~/Workspace/gitrepo/dotfiles/_emacs.d.25.1.osx/

# push updates to github.com
cd ~/Workspace/gitrepo/dotfiles
git add -A
git commit -m "update emacs config from mac"
git push
cd ~