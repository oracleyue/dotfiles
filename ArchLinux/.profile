#!/bin/sh
# Profile file. Runs on login.

# Source ".bashrc"
[ -f ~/.bashrc ] && source "$HOME/.bashrc"

# Start ssh-agent to avoid repeating passphrase
eval "$(ssh-agent -s)"
ssh-add < /dev/null
