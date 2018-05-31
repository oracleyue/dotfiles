;; ===============================================================
;; General Programming Supports
;; ===============================================================


;; ---------------------------------------------
;; /magit/: version control
;; ---------------------------------------------
(if *use-helm*
    (global-set-key (kbd "C-c h g") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))


;; ----------------------------------------------
;; /linum/: adding line number on left fringe
;; ----------------------------------------------
;; config built-in "display-line-number-mode" (require Emacs >= 26)
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 2)
(setq display-line-numbers-current-absolute t)
;; advanced linum style
(use-package linum-highlight-current-line-number
  :config
  (setq linum-format 'linum-highlight-current-line-number))
;; enable line numbering (or "linum-mode")
(let ((hook-list '(sh-mode-hook
                   cmake-mode-hook
                   matlab-mode-hook
                   python-mode-hook
                   c-mode-common-hook
                   makefile-gmake-mode-hook   ; Gnome
                   makefile-bsdmake-mode-hook ; OS X
                   ess-mode-hook)))  ; R
  (dolist (hook-element hook-list)
    (add-hook hook-element 'display-line-numbers-mode)))


;; ----------------------------------------------
;; /iedit/: edit the same variable everywhere (keystroke "C-c ;")
;; ----------------------------------------------
(require 'iedit)
(when (eq 'monokai (car custom-enabled-themes))
  (set-face-attribute 'iedit-occurrence nil
                      :foreground "#272822"
                      :background "#E6DB74"
                      :weight 'normal))


;; ----------------------------------------------
;; /ediff/: comparing two files
;; ----------------------------------------------
(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)


;; ----------------------------------------------
;; Semantic parsing and overview
;; ----------------------------------------------
;; /semantic/ (CEDET)
;; required by ~helm/ivy-semantic-or-imenu~ and /stickyfunc/
(when *enable-semantics*
  (semantic-mode 1)
  (semantic-default-elisp-setup)
  ;; enable  minimal /semantic/ for /stickyfunc/ and /*-semantic-or-imenu/
  ;; stop semantic parsing (huge,slow) elisp sys libraries
  (setq-mode-local emacs-lisp-mode
                   semanticdb-find-default-throttle
                   (default-value 'semanticdb-find-default-throttle))

  ;; /stickyfunc/ shows the function name on top of the buffer
  (add-hook 'prog-mode-hook 'global-semantic-stickyfunc-mode)
  (require 'stickyfunc-enhance))


;; ----------------------------------------------
;; API reference support
;; ----------------------------------------------
;; Integration with /Dash/ for quick refernce (only available for Mac OS X)
(when (string-equal system-type "darwin")
  (require 'dash-at-point)
  (global-set-key (kbd "C-c d") 'dash-at-point) ;; "C-c d", conflicts with /doxyemacs/
  ; specify docsets to search in different modes
  (set 'dash-at-point-mode-alist
       '((c-mode . "c,gsl,gl4")
         (c++-mode . "cpp,eigen,boost,gsl")
         (python-mode . "python,numpy,scipy,matplotlib,pandas")
         (ess-mode . "r")
         (sh-mode . "bash"))))
;; Integration with /Zeal/ for quick refernce (available for Linux)
(when (string-equal system-type "gnu/linux")
  (require 'zeal-at-point)
  (global-set-key (kbd "C-c d") 'zeal-at-point)
  (set 'dash-at-point-mode-alist
       '((c-mode . "c,gsl,gl4")
         (c++-mode . "c++,eigen,boost,gsl")
         (python-mode . "python 2,numpy,scipy,matplotlib,pandas")
         (ess-mode . "r"))))


;; ----------------------------------------------
;; /gud/: debug supports, e.g. gdb, pdb
;; ----------------------------------------------
;; - usages: "M-x pdb", "M-x gud-gdb"
;;           "M-x gdb" then "M-x gdb-many-window", then "b main" "r"
;; - notes: keybindings compatible with JetBrains
(when (string-equal system-type "darwin")
  (setq gdb-non-stop-setting nil))
(global-set-key (kbd "<f7>") 'gud-step);; equiv step in
(global-set-key (kbd "<f8>") 'gud-next) ;; equiv step over
(global-set-key (kbd "<f9>") 'gud-cont) ;; equiv continue
(global-set-key (kbd "S-<f8>") 'gud-finish) ;; equiv step out


;; ----------------------------------------------
;; /ECB/: GUI interface of IDE
;; ----------------------------------------------
(when *enable-ecb*
  (add-to-list 'load-path "~/.emacs.d/git/ecb")
  (require 'ecb)
  (require 'ecb-autoloads)
  (setq ecb-windows-width 0.16)
  (setq ede-project-directories (quote ("~/Workspace/c")))
  (setq ecb-source-path '("~/Workspace/c/src"
                          "~/Workspace/c/include"))
  (setq ecb-tip-of-the-day nil)
  (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

  (add-hook 'c-mode-hook 'ecb-activate)
  (add-hook 'c++-mode-hook 'ecb-activate)
  (add-hook 'matlab-mode-hook 'ecb-activate))


;; ----------------------------------------------
;; /projectile/: global config (besides helm/counsel-projectile)
;; ----------------------------------------------
;; Do not visit the current project's tags table if `helm-projectile-mode' or
;; 'counsel-projectile-mode' is loaded.  Doing so prevents the unnecessary call
;; to `visit-tags-table' function and the subsequent `find-file' call for the
;; `TAGS' file."
(defun zyue/advice-projectile-dont-visit-tags-table ()
  "Don't visit the tags table as we are using gtags/global."
  nil)
(when (or (fboundp 'helm-gtags) (fboundp 'counsel-gtags-mode))
  (advice-add 'projectile-visit-project-tags-table :override
              #'zyue/advice-projectile-dont-visit-tags-table))



(provide 'init-progtools)
;; ================================================
;; init-progtools.el ends here
