;; ===============================================================
;; General Programming Supports
;; ===============================================================
;; Last modified on 20 Feb 2020


;; ---------------------------------------------
;; Basics of prog-mode
;; ---------------------------------------------

(defun zyue/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.2))
  (redraw-frame (selected-frame)))

;; line wrapping
;; note: "wrap at window edge" cause issues in company
(add-hook 'prog-mode-hook
          (lambda () (setq-default truncate-lines t)
            (setq fill-column *fill-column-mono*)))

;; ---------------------------------------------
;; /flycheck/: modern syntax checking
;; ---------------------------------------------
;; loaded in "init-basic.el"; here add mode-hooks
(use-package flycheck
  :diminish
  :hook ((c-mode      . flycheck-mode)
         (c++-mode    . flycheck-mode)
         (ess-mode    . flycheck-mode)
         (python-mode . flycheck-mode)))

;; ---------------------------------------------
;; /magit/: version control
;; ---------------------------------------------
(use-package magit :demand)
;; use "M-x magit" or "magit-status"

;; ----------------------------------------------
;; line numbering
;; ----------------------------------------------
;; use built-in "display-line-number-mode" (require Emacs >= 26)
(if *is-terminal*
    ;; choose basic line numbering in ssh terminal
    (setq linum-format "%4d ")
  ;; graphic display
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
  )

;; ----------------------------------------------
;; /iedit/: edit the same variable everywhere (keystroke "C-c ;")
;; ----------------------------------------------
(use-package iedit :demand)

;; ----------------------------------------------
;; /symbol-overlay/: highlight symbols to improve readability
;; ----------------------------------------------
(use-package symbol-overlay
  :demand
  :diminish
  :bind (("M-I" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)))

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
  (use-package stickyfunc-enhance
    :config
    (add-hook 'prog-mode-hook 'global-semantic-stickyfunc-mode))
  )

;; ----------------------------------------------
;; API reference support
;; ----------------------------------------------
;; Integration with /Dash/ for quick refernce (only available for Mac OS X)
(when *is-mac*
  (use-package dash-at-point
    :demand
    :config
    (global-set-key (kbd "C-c d") 'dash-at-point) ;; "C-c d", conflicts with /doxyemacs/
                                        ; specify docsets to search in different modes
    (set 'dash-at-point-mode-alist
         '((c-mode . "c,gsl,gl4")
           (c++-mode . "cpp,eigen,boost,gsl")
           (python-mode . "python,numpy,scipy,matplotlib,pandas")
           (ess-mode . "r")
           (sh-mode . "bash"))))
  )
;; Integration with /Zeal/ for quick refernce (available for Linux)
(when *is-linux*
  (use-package zeal-at-point
    :demand
    :config
    (global-set-key (kbd "C-c d") 'zeal-at-point)
    (set 'dash-at-point-mode-alist
         '((c-mode . "c,gsl,gl4")
           (c++-mode . "c++,eigen,boost,gsl")
           (python-mode . "python 2,numpy,scipy,matplotlib,pandas")
           (ess-mode . "r"))))
  )

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
  (add-to-list 'load-path "~/.emacs.d/site-lisp/ecb")
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


(provide 'init-programming)
;; ================================================
;; init-programming.el ends here
