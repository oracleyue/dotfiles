#!/bin/sh
# Profile file. Runs on login.

# Source ".bashrc"
[ -f ~/.bashrc ] && source "$HOME/.bashrc"

# Enable Chinese for certain apps in i3
export LANG="en_US.UTF-8"
export LC_CTYPE="zh_CN.UTF-8"
