#!/bin/bash

echo ">>> .vimrc"
rsync -aP ~/.vimrc local:/home/zyue/
echo "<<<"
echo 
echo ">>> nvim"
rsync -aP ~/.config/nvim/ --exclude=.git local:/home/zyue/.config/nvim/
echo "<<<"
