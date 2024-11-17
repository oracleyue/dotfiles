#!/bin/bash

# choose job
# job="dl"
job="rename"

# url
baseURL="https://bimsa.net/bimsavideo.php?id="
tempNamePrefix="bimsavideo-bimsavideo.phpid="

# items
dlList="list.csv"
renamePrefix='sml-II-'

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
        # rename
        # mv ${tempNamePrefix}${id}.php ${id}.mp4
        echo

        index=$((index+1))
    done
    ;;
# -------------------------------------------
# Update file names
# -------------------------------------------
  "rename")
    # list the old and new names
    echo "List of videos to be renamed: [OLD >>> NEW]"
    echo "--------------------------------------"
    index=0
    while [[ $index -lt $maxIndex ]]
    do
        id="${ids[$index]}"
        name="${names[$index]}"
        echo "id: $id    >>>    p$((index+1)).$renamePrefix$name.mp4"
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
            # renaming
            # mv "${id}.mp4" "p$((index+1)).$renamePrefix$name.mp4"
            # no renaming
            mv "${tempNamePrefix}${id}.php" \
               "p$((index+1)).$renamePrefix$name.mp4"

            index=$((index+1))
        done
        echo "video file renaming [done]"
    fi
    ;;

esac
