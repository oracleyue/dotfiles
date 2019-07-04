#!/bin/bash
# -*- coding: utf-8 -*-
#
# Compare the current Emacs config with the github repo.

# Copyright (c) 2014-2017, Zuogong YUE
# Licensed under the GNU General Public License
#
# Last modified on 06 Jun 2019

localPath=${HOME}/.emacs.d/init
repoPath=${HOME}/Workspace/gitrepo/dotfiles/_emacs.d/init
tmpfile="_diff.tmp"

echo "init.el" > $tmpfile
echo "----------------" >> $tmpfile
diff $localPath/../init.el $repoPath/../init.el >> $tmpfile
echo "" >> $tmpfile
for file in $localPath/*.el; do
    fname=${file##*/}
    echo $fname >> $tmpfile
    echo "----------------" >> $tmpfile
    diff $localPath/$fname $repoPath/$fname >> $tmpfile
    echo "" >> $tmpfile
done

cat $tmpfile
