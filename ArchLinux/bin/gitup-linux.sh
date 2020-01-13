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
repopath=$HOME'/Workspace/gitrepo/dotfiles/ArchLinux'

# List of rsync files

# /etc config files
# ftp server: vsFTP
$sync /etc/vsftpd.conf $repopath/ROOT/etc/
# ssh server: openssh
$sync /etc/ssh/sshd_config $repopath/ROOT/etc/ssh/
# login banner (welcome message)
$sync /etc/profile.d/motd.sh $repopath/ROOT/etc/profile.d/
# bashrc
$sync ~/.bashrc       $repopath
$sync ~/.bash_profile $repopath
$sync ~/.profile      $repopath

# bin scripts
$sync ~/bin    $repopath

# vim
$sync ~/.vimrc $repopath
$sync ~/.vim   $repopath

# X configs
$sync ~/.Xmodmap*    $repopath
$sync ~/.xprofile    $repopath
$sync ~/.Xresources* $repopath
$sync ~/.xinitrc     $repopath

# git config
$sync ~/.gitconfig $repopath

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
# font config
$sync ~/.config/fontconfig $repopath/.config
# mimeapps.list
$sync ~/.config/mimeapps.list $repopath/.config
# i3 and i3blocks (window manager)
$sync ~/.config/i3       $repopath/.config
$sync ~/.config/i3status $repopath/.config
# conky as desktop widgets to show help/info
$sync ~/.config/conky    $repopath/.config
# file manager ranger
$sync --exclude=__* ~/.config/ranger $repopath/.config
# sxhkd
$sync ~/.config/sxhkd    $repopath/.config
# zathura
$sync ~/.config/zathura  $repopath/.config
# sxiv for key-handler
$sync ~/.config/sxiv     $repopath/.config
# mpd; ncmpcpp (music player demo and client)
$sync ~/.config/mpd/mpd.conf     $repopath/.config/mpd
$sync ~/.config/ncmpcpp/bindings $repopath/.config/ncmpcpp
$sync ~/.config/ncmpcpp/config   $repopath/.config/ncmpcpp
# aria2 for downloading
$sync ~/.config/aria2    $repopath/.config
# vscode
$sync ~/.config/Code\ -\ OSS/User/settings.json $repopath/.config/Code\ -\ OSS/User/
$sync ~/.config/Code\ -\ OSS/User/snippets $repopath/.config/Code\ -\ OSS/User/
# qtcreator
$sync ~/.config/QtProject/QtCreator.ini $repopath/.config/QtProject/
$sync ~/.config/QtProject/qtcreator/styles $repopath/.config/QtProject/qtcreator/
# smplayer
$sync ~/.config/smplayer/smplayer.ini $repopath/.config/smplayer
$sync ~/.config/smplayer/styles.ass $repopath/.config/smplayer
$sync ~/.config/smplayer/player_info.ini $repopath/.config/smplayer
$sync ~/.config/smplayer/device_info.ini $repopath/.config/smplayer
$sync ~/.config/smplayer/hdpi.ini $repopath/.config/smplayer
# gtk and kde/qt settings
# use KDE's System Settings for Qt apps; one may also use it for gtk application styles
$sync ~/.gtkrc-2.0         $repopath
$sync ~/.config/gtk-3.0    $repopath/.config
$sync ~/.config/kdeglobals $repopath/.config

# $HOME/.local
# desktop shortcuts
$sync ~/.local/share/applications/*.desktop $repopath/.local/share/applications

# Pull updates first from github.com
cd $repopath
git add -A
#git ci -m "backup Arch linux configs"
git ci
git push
