#!/bin/bash

listURL=""

#for index in 19 25 27 28 30 31
#cat ../failed_items.txt | while read index
for index in {1..66}
do
    url="$listURL?p="${index}
    echo $url
    echo ">>>"
    # downloading video
    yt-dlp --cookies ~/Downloads/cookies.txt --format 'bestvideo[height>=?720][ext=mp4]+worstaudio[ext=m4a]' $url
    #yt-dlp --cookies ~/Downloads/cookies.txt --format 'bestvideo[ext=mp4]+worstaudio[ext=m4a]' $url

    # retriving video metadata
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
