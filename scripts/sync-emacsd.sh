#!/bin/bash
# This is to sync ~/.emacs.d to the current folder.

# emacs for terminal
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeTerm/

# emacs with dark themes or using auto-complete
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeDark/
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d ~/bin/HomeAutoCompl/

# backup current .emacs .emacs.d/ to "~/Files/config/emacs-config/backup/"
cp -f ~/.emacs ~/Software/config/emacs-config/backup/dot_emacs
rsync -rlptD -P --exclude=.DS_Store --delete ~/.emacs.d/ ~/Software/config/emacs-config/backup/dot_emacs.d
