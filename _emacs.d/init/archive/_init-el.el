#!/usr/bin/env emacs --script

(require 'ob-tangle)

(setq prepath (expand-file-name default-directory))
(let ((files (directory-files
              (concat user-emacs-directory "init/") nil "\\.org$")))
  (dolist (file files)
    (setq file-dest (concat prepath
                            (file-name-sans-extension file) ".el"))
    (setq file (concat  prepath file))
    (org-babel-tangle-file file file-dest "emacs-lisp")))
