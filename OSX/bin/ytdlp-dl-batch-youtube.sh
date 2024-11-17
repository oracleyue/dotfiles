#!/bin/bash

urlList=(
    "https://youtu.be/Rbm0YfqEJlo"
    "https://youtu.be/NCb_Qo81rU0"
    "https://youtu.be/XU-zh0ORYRg"
    "https://youtu.be/VYJ7TUgIcNs"
    "https://youtu.be/brAu3fnJzq8"
    "https://youtu.be/DPB_WkB6bQI"
)

for url in "${urlList[@]}"
do
    echo $url
    echo ">>>video:"
    # downloading videos
    yt-dlp --cookies ~/Downloads/cookies.txt \
       --proxy "socks5://127.0.0.1:7890" \
       --format 'bestvideo[ext=mp4]+worstaudio[ext=m4a]' \
       $url
    echo

    # retrieve metada
    echo ">>>metadata:"
    yt-dlp $url --write-info-json --skip-download --proxy "socks5://127.0.0.1:7890"
    echo ">>>done"
    echo
done
