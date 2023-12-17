#!/bin/bash

file=$(ls *.pdf)

/usr/bin/open $file
# time passes
/usr/bin/osascript -e 'tell application "Preview" to close document "$file"'
