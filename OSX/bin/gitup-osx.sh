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
$sync ~/.bash_profile $repopath
$sync ~/.tmux.conf* $repopath
$sync ~/.screenrc $repopath

# ===========================================
# config files under $HOME
# ===========================================
$sync ~/.gitconfig $repopath
$sync ~/.globalrc $repopath
$sync ~/.Xresources $repopath
$sync ~/.latexmkrc $repopath
$sync ~/.Renviron $repopath
$sync ~/.Rprofile $repopath
$sync ~/.jupyter $repopath
$sync ~/.ctags $repopath

# ===========================================
# template files
# ===========================================
$sync ~/Workspace/templates $repopath

# ===========================================
# apps
# ===========================================

# VIM
$sync ~/.vimrc $repopath
$sync ~/.vim --exclude=bundle $repopath

# Vimperator for Firefox
$sync ~/.vimperatorrc $repopath/apps/vimperator/_vimperatorrc
$sync ~/.vimperator/colors/oracleyue-dark.vimp \
      $repopath/apps/vimperator/oracleyue-dark.vimp

# Sublime Text
sublpath="$HOME/Library/Application Support/Sublime Text 3/Packages/User"
cp "$sublpath"/*.sublime-keymap $repopath/apps/sublime-text/
cp "$sublpath"/*.sublime-settings $repopath/apps/sublime-text/

# vscode
vscpath="$HOME/Library/Application Support/Code/User"
cp "$vscpath"/settings.json $repopath/apps/vscode/
$sync "$vscpath"/snippets $repopath/apps/vscode/

# ===========================================
# "~/bin" on OSX
# ===========================================
$sync --exclude-from="$HOME/bin/exclude-list" --delete ~/bin $repopath

# ===========================================
# "~/Library" on OSX
# ===========================================
$sync ~/Library/texmf $repopath/Library

# ===========================================
# push updates to github.com
# ===========================================
cd $repopath
git add -A
git commit -m "update config from mac"
git push
cd ~
