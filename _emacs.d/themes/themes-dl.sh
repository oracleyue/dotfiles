#!/bin/bash

# This script is to clone necessary themes from github.com.


if [[ -d github ]]; then
    cd github
else
    mkdir github
    cd github
fi

# list of clones
git clone https://github.com/fuxialexander/emacs-doom-themes
git clone https://github.com/abo-abo/eclipse-theme

