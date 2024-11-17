#!/bin/bash
# This script converts all png/jpg images into PDF and combine them into
# one pdf file. It converts png to pdf without resizing.
#
# Last modified on 10 Jun 2020


# check arguments
if [ $# -eq 0 ]; then
    echo "Usage: img2pdf_combined [arg1] [arg2](optional)"
    echo "- arg1: jpg, png, webp"
    echo "- arg2: image resolution; default 200"
    exit 1
elif [ $# -eq 1 ]; then
    ext=$1
    density=200
elif [ $# -eq 2 ]; then
    ext=$1
    density=$2
else
    echo "Too many arguments!"
    exit 1
fi

# check if output pdf file exists
outfile="all-in-one.pdf"
if [ -f $outfile ]; then
    echo "File $outfile exists. Remove it before processing!"
    exit 1
fi

# convert images if required
case $ext in
    webp)
        find ./ -name "*.webp" -exec dwebp {} -o {}.png \;
        ext='png'
        ;;
    *)
        ;;
esac

# convert img to pdf
for img in ./*.${ext}; do
    convert $img -density $density -quality 100  $img.pdf
    echo "... ${img} processed"
done

# combine pdf
pdftk *.pdf cat output $outfile

# remove intermediate files
case $ext in
    png | PNG)
        rm *.webp.png
        rm *.png.pdf
        rm *.PNG.pdf
        ;;
    jpg | JPG)
        rm *.jpg.pdf
        rm *.JPG.pdf
        ;;
    jpeg | JPEG)
        rm *.jpeg.pdf
        rm *.JPEG.pdf
        ;;
esac
