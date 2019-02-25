#!/bin/bash

# This script is to clone necessary themes from github.com.


if [ -d github ]; then
    cd github
else
    mkdir github
fi
cd github

# list of clones
git clone https://github.com/fuxialexander/emacs-doom-themes

