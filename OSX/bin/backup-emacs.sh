#!/bin/bash
# This is to backup .emacs .emacs.d/ to "~/Software/config/emacs-config/backup/".


# backup .emacs.d
rsync -rlptD -P --exclude=.DS_Store --delete \
      ~/.emacs.d/ ~/Software/config/emacs-config/backup/dot_emacs.d
