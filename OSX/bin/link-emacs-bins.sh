#!/bin/bash
# This is to link executable files of Emacs.app installed from https://emacsformacosx.com/ to provide terminal supports.

cd /usr/local/bin
ln -s /Applications/Emacs.app/Contents/MacOS/Emacs emacs
ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
ln -s /Applications/Emacs.app/Contents/MacOS/bin/etags
ln -s /Applications/Emacs.app/Contents/MacOS/bin/ebrowse
