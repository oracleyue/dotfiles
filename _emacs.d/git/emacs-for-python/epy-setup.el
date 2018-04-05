;; epy-setup.el - setup and load all the paths necessary
;; note: modified by oracleyue on 24 Aug 2017.

(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python terminating with a slash"
  )


;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   ;; "extensions/yasnippet"            ;oracleyue
                   ;; "extensions/auto-complete"
                   ;; "extensions/eproject"
                   )
                 )
  (add-to-list 'load-path (concat epy-install-dir relpath)))

(when (not (boundp 'yas--version))
  (setq yas--version nil))

(defvar epy-yas--version yas--version
  "Version of yassnippet found when epy was loaded.

Not used now, just for debugging. ")

(defcustom epy-load-yasnippet-p nil
 "If yasnippet stuff delivered here should be loaded.

Default is nil.
Disable in case of versions-conflicts etc. "

:type 'boolean
:group 'python-mode)

(defvar epy-enable-ropemacs t
  "Variable to control whether enable setup-ropemacs() in epy-python.el.")

(provide 'epy-setup)
