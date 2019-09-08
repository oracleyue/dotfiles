#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize dot configuration files under "~/"
# to the repository on github.com.
#
# !! This is for Mac OS X.

# Copyright [2017] <oracleyue>


# ===========================================
# essential settings
# ===========================================
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME/Workspace/gitrepo/dotfiles/OSX

# ===========================================
# bash
# ===========================================
$sync ~/.bashrc $repopath
$sync ~/.tmux.conf* $repopath
$sync ~/.screenrc $repopath

# ===========================================
# config files under $HOME
# ===========================================
$sync ~/.latexmkrc $repopath
$sync ~/.Renviron $repopath
$sync ~/.Rprofile $repopath
$sync ~/.jupyter/ $repopath

# ===========================================
# editors
# ===========================================

# VIM
$sync ~/.vimrc $repopath
$sync ~/.vim/ --exclude=bundle $repopath

# Vimperator for Firefox
$sync ~/.vimperatorrc $repopath/apps/vimperator/_vimperatorrc
$sync ~/.vimperator/colors/oracleyue-dark.vimp \
      $repopath/apps/vimperator/oracleyue-dark.vimp

# Sublime Text
sublpath="$HOME/Library/Application Support/Sublime Text 3/Packages/User"
cp "$sublpath/"*.sublime-keymap $repopath/apps/sublime-text/
cp "$sublpath/"*.sublime-settings $repopath/apps/sublime-text/

# ===========================================
# "~/bin" on OSX
# ===========================================
$sync --exclude-from="$HOME/bin/exclude-list" --delete ~/bin $repopath

# ===========================================
# "~/Library/texmf" on OSX
# ===========================================
cd $repopath/config
tar -czf texmf.tar.gz -C ~/Library/texmf .

# ===========================================
# push updates to github.com
# ===========================================
cd $repopath
git add -A
git commit -m "update config from mac"
git push
cd ~
