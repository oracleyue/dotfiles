#!/bin/bash

listURL=""

#for index in 20 23 24 26 29
#do
    #url="https://www.bilibili.com/video/BV1J64y1F7nW?p="${index}
    #echo $url
    #echo ">>>"
    #you-get --format=dash-flv $url
    #mv *.mp4 ../${index}.mp4
    #echo "done<<<"
    #echo
#done

#for index in 19 25 27 28 30 31
#cat ../failed_items.txt | while read index
for index in {1..27}
do
    url="$listURL?p="${index}
    echo $url
    echo ">>>"
    # Bilibili
    you-get --format=dash-flv -c ~/Downloads/cookies.txt $url
    # Youtube
    # yt-dlp --format 'best[ext=mp4]' --playlist-items $index $listURL

    # retriving video metadata
    you-get -u -c ~/Downloads/cookies.txt $url >> ../you-get.log

    # renaming files if filename is too long
    mv *.mp4 ../P${index}.mp4
    if [ $? -ne 0 ]; then
        echo ${index} >> ../failed_items.txt
    fi

    echo "P${index}: done<<<"
    echo
done

# Retrieve metadata for the whole list
# you-get -u -c ~/Downloads/cookies.txt -l $listURL | tee ../you-get.log
