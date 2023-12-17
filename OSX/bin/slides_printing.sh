#!/bin/bash

while read -r file
do
    :

    # Print 4 pages into one for each PDF
    #echo "${file%.pdf}_4p.pdf" >> list_4p.txt
    #pdfjam $file -o ${file%.pdf}_4p.pdf --nup 2x2 --landscape

    # Print the numbers of pages in each PDF
    # pnum=$(identify $file 2>/dev/null | wc -l | tr -d ' ')
    # echo "$file:$pnum" >> list_addblank.txt
done < list_4p.txt

pdftk $(cat list_addblank.txt) cat output FULL_lectures.pdf
