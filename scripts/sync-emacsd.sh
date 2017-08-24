#!/bin/bash
# This is to sync ~/.emacs.d to the pseudo-home folders.

# emacs for terminal
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeTerm/

# emacs with dark themes
sed 's/Mono-13/Mono-15/' ~/.emacs > ~/bin/HomeDark/.emacs
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeDark/

# emacs with auto-complete
sed 's/Mono-13/Mono-15/' ~/.emacs > ~/bin/HomeDark/.emacs
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeAutoCompl/
