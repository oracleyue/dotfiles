#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to backup essential Arch Linux settings for quick OS
# installation later.

# Copyright (c) 2014-2019, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on22 Jun 2019


sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/dotfiles/ArchLinux/'

# List of rsync files

# bashrc
$sync ~/.bashrc $repopath
$sync ~/.bash_profile $repopath

# bin scripts
$sync ~/bin $repopath

# vim
$sync ~/.vimrc $repopath
$sync ~/.vim $repopath

# X configs
$sync ~/.Xmodmap* $repopath
$sync ~/.xprofile $repopath
$sync ~/.Xresources $repopath
$sync ~/.xinitrc $repopath

# latexmk
$sync ~/.latexmkrc $repopath

# R
$sync ~/.Renviron $repopath
$sync ~/.Rprofile $repopath

# GNU Global (one may update by copying from /usr/share/gtags/gtags.conf)
$sync ~/.globalrc $repopath

# Desktop files in ~/.local/share/applications/
$sync ~/.local/share/applications/emacs*.desktop $repopath/applications
$sync ~/.local/share/applications/matlab*.desktop $repopath/applications

# Thunderbird user config files (~/.thunderbird/PROFILE_NAME/chrome/)
$sync ~/.thunderbird/*.default/chrome $repopath

# Pull updates first from github.com
cd $repopath
git add -A && git ci -m "backup Arch linux configs" && git push
