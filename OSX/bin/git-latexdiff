#!/bin/bash

TMPDIR=$(mktemp -d ~/tmp/git-latexdiff.XXXXXXXX)
WORKDIR=$(pwd)
FILEPATH=$(dirname "$2")
cd $TMPDIR
ln -s $WORKDIR/$FILEPATH/figures
ln -s $WORKDIR/$FILEPATH/ref
ln -s $WORKDIR/$FILEPATH/images
ln -s $WORKDIR/$FILEPATH/userdef-mathsymb.tex
cd $WORKDIR
latexdiff "$1" "$2" > $TMPDIR/diff.tex
# pdflatex -interaction nonstopmode -output-directory $TMPDIR $TMPDIR/diff.tex
cd $TMPDIR; rubber -fd diff.tex; rubber -fd diff.tex
open $TMPDIR/diff.pdf
rm -rf $TMPDIR