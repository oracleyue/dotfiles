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

# $HOME/.config
# systemd services
$sync ~/.config/systemd/user/*.service $repopath/.config/systemd/user
# i3 and i3blocks (window manager)
$sync ~/.config/i3       $repopath/.config
$sync ~/.config/i3blocks $repopath/.config
# vifm (file manager)
$sync ~/.config/vifm     $repopath/.config

# Desktop shortcuts
$sync ~/.local/share/applications/*.desktop $repopath/.local/share/applications

# Pull updates first from github.com
cd $repopath
git add -A && git ci -m "backup Arch linux configs" && git push
