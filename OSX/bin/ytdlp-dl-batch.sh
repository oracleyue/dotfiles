#!/bin/bash

urlList=(
    "https://www.bilibili.com/video/BV1j5411X7h4"
    "https://www.bilibili.com/video/BV1LA4y1Z7o5"
    "https://www.bilibili.com/video/BV12t4y1s7Nw"
)

for url in "${urlList[@]}"
do
    echo $url
    echo ">>>"
    # downloading videos
    yt-dlp \
        --format 'bestvideo[ext=mp4]+worstaudio[ext=m4a]' $url
        # --cookies ~/Downloads/cookies.txt

    echo ">>> done"
    echo
done
