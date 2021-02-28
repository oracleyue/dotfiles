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
;; warning: ensure /epl/ package being up-to-date
(use-package flycheck
  :demand
  :diminish "FlyC"
  :hook ((c-mode      . flycheck-mode)
         (c++-mode    . flycheck-mode)
         (ess-mode    . flycheck-mode)
         (python-mode . flycheck-mode))
  :bind (("M-g n" . flycheck-next-error)
         ("M-g p" . flycheck-previous-error)
         ("M-g l" . flycheck-list-errors))
  :config
  ;; check only when opening or saving files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; ---------------------------------------------
;; /magit/: version control
;; ---------------------------------------------
(use-package magit :demand)
;; use "M-x magit" or "magit-status"

;; ----------------------------------------------
;; line numbering
;; ----------------------------------------------
;; use built-in "display-line-number-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :init
  (setq-default display-line-numbers-width 2)
  ;; (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-current-absolute t)
  ;; set line-number background
  ;; (set-face-background 'line-number (face-background 'org-block))
  ;; current line-number highlight
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

;; use another old built-in "linum-mode"
;; (when *is-terminal*
;;   (setq linum-format "%4d "))

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
         ("M-C" . symbol-overlay-remove-all))
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


(provide 'init-programming)
;; ================================================
;; init-programming.el ends here
