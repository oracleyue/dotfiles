#!/bin/bash

listURL=""

# download videos
yt-dlp --cookies ~/Downloads/cookies.txt \
       --proxy "socks5://127.0.0.1:7890" \
       --format 'bestvideo[ext=mp4]+worstaudio[ext=m4a]' \
       --playlist 1-1 $listURL

# --format 'best[ext=mp4]' \
# --format 'bestvideo[height>=?720][ext=mp4]+worstaudio[ext=m4a]' \ 

# retrieve metada
yt-dlp $listURL --write-info-json --skip-download --proxy "socks5://127.0.0.1:7890" 

