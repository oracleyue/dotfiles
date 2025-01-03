#!/bin/bash

# choose job
# job="dl"
job="rename"

# url
baseURL="https://bimsa.net/class/postVideo.php?id="
tempNamePrefix="postVideo-postVideo.phpid="

# items
dlList="list.csv"   # Format: id, date
renamePrefix='bm-'

ids=($(awk -F, '{print $1}' $dlList))
names=($(awk -F, '{print $2}' $dlList))
maxIndex=${#ids[*]}
echo "Total number of items: $maxIndex"

echo "Job [$job] starts ..."
case $job in
# -------------------------------------------
# Downloading
# -------------------------------------------
  "dl")
    index=0
    while [[ $index -lt $maxIndex ]]
    do
        id="${ids[$index]}"
        url="$baseURL${id}.mp4"
        echo $url
        # downloading
        echo ">>> [$((index+1))/$maxIndex]"
        youtube-dl -f "best" $url
        echo "${id}.mp4: done<<<"
        echo

        index=$((index+1))
    done
    ;;
# -------------------------------------------
# Update file names
# -------------------------------------------
  "rename")
    # append to the last downloading index
    startIndex=8  # the last index; default: 0
    # list the old and new names
    echo "List of videos to be renamed: [OLD >>> NEW]"
    echo "--------------------------------------"
    index=0
    while [[ $index -lt $maxIndex ]]
    do
        id="${ids[$index]}"
        name="${names[$index]}"
        echo "id: $id    >>>    p$((startIndex+index+1)).$renamePrefix$name.mp4"
        index=$((index+1))
    done

    # confirm to perform
    read -p "Are you sure? " -n 1 -r
    echo
    index=0
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        while [[ $index -lt $maxIndex ]]
        do
            id="${ids[$index]}"
            name="${names[$index]}"
            mv "${tempNamePrefix}${id}.php" \
               "p$((startIndex+index+1)).$renamePrefix$name.mp4"

            index=$((index+1))
        done
        echo "video file renaming [done]"
    fi
    ;;

esac
