#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize dot configuration files under "~/"
# to the repository on github.com.
#
# !! This is for Mac OS X.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/dotfiles'


# rsync bash config
$sync ~/.bashrc $repopath/bash/_bashrc.osx
$sync ~/.tmux.conf $repopath/tmux/_tmux.conf.osx
$sync ~/.screenrc $repopath/tmux/_screenrc.osx

# rsync tool config
$sync ~/.latexmkrc $repopath/bash/_latexmkrc
$sync ~/.Renviron $repopath/bash/_Renviron.osx
$sync ~/.Rprofile $repopath/bash/_Rprofile.osx
$sync ~/.jupyter/ $repopath/bash/_jupyter.osx

# rsync editor config
## VIM
$sync ~/.vimrc $repopath/editors/_vimrc
$sync ~/.vim/ --exclude=bundle $repopath/editors/_vim

## Vimperator for Firefox
$sync ~/.vimperatorrc $repopath/editors/_vimperatorrc
$sync ~/.vimperator/colors/oracleyue-dark.vimp \
      $repopath/editors/oracleyue-dark.vimp

## Sublime Text
$sync ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Default\ \(OSX\).sublime-keymap $repopath/editors/Default\ \(OSX\).sublime-keymap

# rsync scripts
$sync ~/bin/gitup-dotfiles.sh $repopath/scripts/
$sync ~/bin/gitup-emacs.sh $repopath/scripts/
$sync ~/bin/gitpull-emacs.sh $repopath/scripts/
$sync ~/bin/emacs-server-func.sh $repopath/scripts/


# push updates to github.com
cd $repopath
git add -A
git commit -m "update emacs config from mac"
git push
cd ~
