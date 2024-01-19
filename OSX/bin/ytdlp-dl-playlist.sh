#!/bin/bash

listURL=""

#for index in 19 25 27 28 30 31
#cat ../failed_items.txt | while read index
for index in {1..25}
do
    url="$listURL?p="${index}  # bilibili
    echo $url
    echo ">>>"
    # downloading videos
    yt-dlp --cookies ~/Downloads/cookies.txt \
        --format 'bestvideo[ext=mp4]+worstaudio[ext=m4a]' $url
    
    # --format 'bestvideo[height>=?720][ext=mp4]+worstaudio[ext=m4a]'
    # --proxy "socks5://127.0.0.1:7890"

    # retriving metadata
    yt-dlp $url --write-info-json --skip-download 

    # renaming files if filename is too long
    mv *.mp4 ../
    if [ $? -ne 0 ]; then
        echo ${index} >> ../failed_items.txt
    fi
    mv *.json ../

    echo ">>> video P${index}: done"
    echo
done
