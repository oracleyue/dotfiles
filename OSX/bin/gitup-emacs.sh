#!/bin/bash
# -*- coding: utf-8 -*-
#
# This script is to syncronize "~/.emacs" and essentials of "~/.emacs.d/"
# to the repository on github.com.

# Copyright [2017] <oracleyue>


# essential bash settings
sync='/usr/bin/rsync -rlptD -P --delete --exclude=.DS_Store'
repopath=$HOME'/Workspace/gitrepo/_emacs.d'

# rsync .emacs OR init.el
# $sync ~/.emacs $repopath/_emacs.25.1.osx
$sync ~/.emacs.d/init.el $repopath/init.el

# rsync .emacs.d (essential packages)
$sync ~/.emacs.d/init $repopath/
$sync --exclude-from="$HOME/.emacs.d/site-lisp/_exclude-list" \
      ~/.emacs.d/site-lisp $repopath/
$sync ~/.emacs.d/site-lisp/lsp-bridge/langserver/matlab-ls.json \
      $repopath/site-lisp/lsp-langserver/
$sync --exclude="github" \
      ~/.emacs.d/themes $repopath/
$sync ~/.emacs.d/snippets $repopath/
# $sync ~/.emacs.d/templates $repopath/

# deprecated config files (init, snippets)
#$sync ~/.emacs.d/archived $repopath/

# push updates to github.com
cd $repopath  # go to dotfile repo
git add -A
git show --name-only
git status
if [[ "$1" == "commit" || "$1" == "ci" ]]; then
    git commit
fi
if [[ "$1" == "push" || "$1" == "p" ]]; then
    git push
fi
