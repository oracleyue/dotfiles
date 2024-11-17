#!/bin/bash

src_dir="/Volumes/zzyWork"
des_dir="/Volumes/zzyBackup"

echo ">>> FROM: $src_dir/"
echo ">>> TO: $des_dir/"
read -p "Are you sure? " -n 1 -r

if [[ $REPLY =~ ^[Yy]$ ]]; then
    /usr/local/bin/rsync -aP --delete $src_dir/ $des_dir/
    echo ">>> DONE"
else
    echo ">>> QUIT"
fi

