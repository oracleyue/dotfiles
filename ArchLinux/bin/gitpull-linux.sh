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

# Pull updates first from github.com
cd $repopath && git pull && cd ~

# List of files to sync to local $HOME/
$sync $repopath/ ~/

# - bashrc: .bashrc; .bash_profile
# - bin scripts: bin/
# - vim: .vimrc; .vim/
# - X configs: .Xmodmap; .xprofile; .Xresources; .xinitrc
# - latexmk: .latexmkrc
# - R: .Renviron; .Rprofile
# - GNU Global: .globalrc
#   note: one may update by copying /usr/share/gtags/gtags.conf


# Move files (not under $HOME) to the right path

# .desktop files in ~/.local/share/applications/
mv ~/applications/emacs*.desktop ~/.local/share/applications/
mv ~/applications/matlab*.desktop ~/.local/share/applications/
rmdir ~/applications

# Thunderbird user config files (~/.thunderbird/PROFILE_NAME/chrome/)
# ![Warning] you need to copy it manually to the right place.
