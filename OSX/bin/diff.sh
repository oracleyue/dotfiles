#!/bin/bash
# -*- coding: utf-8 -*-

oldFile="OLD.tex"
newFile="NEW.tex"

if [[ "$1" == "clean" ]]; then
    latexmk -c diff.tex
    rm -f diff.tex
else
    # using config (with env ignored)
    latexdiff -c ld.cfg $oldFile $newFile > diff.tex
    # without math (or modify "ld.cfg")
    # latexdiff --encoding=ascii --math-markup=0 $oldFile $newFile > diff.tex

    # compile
    latexmk -f -silent diff.tex
    latexmk -f -silent diff.tex
fi
