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
;; line numbering
;; ----------------------------------------------
;; use built-in "display-line-number-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :init
  (setq-default display-line-numbers-width 2)
  ;; (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-current-absolute t)
  ;; change line-number background
  ;; (set-face-background 'line-number (face-background 'org-block))
  ;; highlight current line
  (set-face-foreground 'line-number-current-line "goldenrod")
  (set-face-bold 'line-number-current-line t)
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
      (add-hook hook-element 'display-line-numbers-mode))))


;; ----------------------------------------------
;; highlight indentation and current line
;; ----------------------------------------------
(defun zyue/edit-hl-config()
  ;; highlight indentation
  (use-package highlight-indent-guides
    :ensure t
    :config
    (setq highlight-indent-guides-method 'character) ;; 'fill, 'column
    ;; tweak colors
    (when (eq zyue-theme 'doom-one)
      (setq highlight-indent-guides-auto-enabled nil)
      (set-face-foreground 'highlight-indent-guides-character-face
                           "#3e6a44a85124"))
    (highlight-indent-guides-mode))
  ;; highlight current line
  (hl-line-mode t))
(add-hook 'python-mode-hook 'zyue/edit-hl-config)


;; ----------------------------------------------
;; /iedit/: edit the same variable everywhere (keystroke "C-c ;")
;; ----------------------------------------------
(use-package iedit :ensure t)


;; ----------------------------------------------
;; /ediff/: comparing two files
;; ----------------------------------------------
(require 'ediff)
;; (setq ediff-diff-options "-w")  ;; ignore white spaces
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
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
;; usages: "M-x pdb"
;;           "M-x gdb" then "M-x gdb-many-window", then "b main" "r"
;; notes: default keybindings
;; =C-c C-s= step in         =C-c C-n= next
;; =C-c C-f= step out        =C-c C-r= continue
;; =C-c C-b= set breakpoint  =C-c C-d= delete breakpoint

(when (string-equal system-type "darwin")
  (setq gdb-non-stop-setting nil))
(require 'gud)
(define-key gud-mode-map (kbd "<f7>") #'gud-next) ;; setp
(define-key gud-mode-map (kbd "<f8>") #'gud-step) ;; setp in
(define-key gud-mode-map (kbd "S-<f8>") #'gud-finish) ;; setp out


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
