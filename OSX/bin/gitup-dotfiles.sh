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
repopath=$HOME/Workspace/gitrepo/dotfiles


# ===========================================
# bash
# ===========================================

$sync ~/.bashrc $repopath/OSX/config/_bashrc
$sync ~/.tmux.conf $repopath/OSX/config/_tmux.conf
$sync ~/.screenrc $repopath/OSX/config/_screenrc


# ===========================================
# config files under $HOME
# ===========================================
$sync ~/.latexmkrc $repopath/bash/_latexmkrc
$sync ~/.latexmkrc $repopath/OSX/config/_latexmkrc
$sync ~/.Renviron $repopath/OSX/config/_Renviron
$sync ~/.Rprofile $repopath/OSX/config/_Rprofile
$sync ~/.jupyter/ $repopath/OSX/config/_jupyter


# ===========================================
# editors
# ===========================================

# VIM
$sync ~/.vimrc $repopath/editors/_vimrc
$sync ~/.vim/ --exclude=bundle $repopath/editors/_vim

# Vimperator for Firefox
$sync ~/.vimperatorrc $repopath/editors/_vimperatorrc
$sync ~/.vimperator/colors/oracleyue-dark.vimp \
      $repopath/editors/oracleyue-dark.vimp

# Sublime Text
sublpath=$HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/
$sync --include="*.sublime-keymap" --exclude="*" $sublpath $repopath/editors/sublime-text/
$sync --include="*.sublime-settings" --exclude="*" $sublpath $repopath/editors/sublime-text/


# ===========================================
# "~/bin" on OSX
# ===========================================
$sync --exclude-from="$HOME/bin/exclude-list" --delete ~/bin $repopath/OSX


# ===========================================
# "~/Library/texmf" on OSX
# ===========================================
cd $repopath/OSX
tar -czf texmf.tar.gz -C ~/Library/texmf .


# ===========================================
# push updates to github.com
# ===========================================
cd $repopath
git add -A
git commit -m "update emacs config from mac"
git push
cd ~
