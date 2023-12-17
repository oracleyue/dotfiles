#!/bin/bash

inputFile="fileList.csv"

index=0
while IFS= read -r line
do
    ((index++))
    echo "$line    >>>    P$index - $line"
    mv "$line" "P$index - $line"

    # # confirm to rename
    # read -p "Are you sure? " -u 1 REPLY
    # if [[ $REPLY =~ ^[Yy]$ ]]; then
    #     mv "$line" "P$index - $line"
    # else
    #     break
    # fi
done < $inputFile
