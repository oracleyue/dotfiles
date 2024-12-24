;; ===============================================================
;; General Programming Supports
;; ===============================================================
;; Last modified on 20 Feb 2020

;; ---------------------------------------------
;; Basics of prog-mode
;; ---------------------------------------------

;; Adding little line spacing
(defun zyue/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.15))
  (redraw-frame (selected-frame)))

;; Line wrapping
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
  :diminish
  :hook ((c-mode      . flycheck-mode)
         (c++-mode    . flycheck-mode)  ;; disable it if use LSP checker only
         (python-mode . flycheck-mode)  ;; disable it if use LSP checker only
         (ess-mode    . flycheck-mode))
  :bind (("M-g n"     . flycheck-next-error)
         ("M-g p"     . flycheck-previous-error)
         ("M-g l"     . flycheck-list-errors))
  :config
  ;; check only when opening or saving files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; ---------------------------------------------
;; /magit/: version control
;; ---------------------------------------------
(use-package magit :demand)
;; use "M-x magit" or "magit-status"

;; ---------------------------------------------
;; /citre/: modern frontend for Universtal Ctags
;; ---------------------------------------------
;; Install by =brew install universal-ctags=
;; To exclude a folder like "Backup", use "ctags --exclude=Backup/* ..."
;; to create and update ctags.
(use-package citre
  :demand
  :diminish
  :functions projectile-project-root
  :bind (:map prog-mode-map
              ("s-."     . citre-jump)
              ("s-,"     . citre-jump-back)
              ;; citre-jump(-back) also reuses xref's "M-." and "M-,"
              ("M-s M-p" . citre-peek)
              ("M-s M-a" . citre-ace-peek)
              ("M-s M-u" . citre-update-this-tags-file)
              ("M-s M-c" . citre-create-tags-file)
              ("M-s M-e" . citre-edit-tags-file-recipe))
  ;; peek window: M-n/-p to move down/up; "M-S-n/-p" to select item; M-l j to jump to definition
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :init
  (setq citre-tags-files '(".tags" "tags"))
  (setq citre-imenu-create-tags-file-threshold 52428800) ;; 50MB for temp tags
  :config
  (setq citre-peek-auto-restore-after-jump nil) ;; close peek window when jump to def
  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref)))

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
                     octave-mode-hook
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
  :bind (:map prog-mode-map
              ("M-I" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)
              ("M-N" . symbol-overlay-switch-forward)
              ("M-P" . symbol-overlay-switch-backward)
              ("M-C" . symbol-overlay-remove-all))
  :hook ((prog-mode      . symbol-overlay-mode)
         (LaTeX-mode     . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode     . (lambda () (symbol-overlay-mode -1)))
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
;; /ivy-dash/: use Ivy to search for Dash.app (requires Alfred)
(use-package ivy-dash
  :disabled
  :ensure nil
  :if *is-mac*
  :load-path "site-lisp"
  :bind (:map prog-mode-map
              ("M-s d" . dash-in-ivy)))
;; /dash-at-point/: search symbol at point in Dash.app
(use-package dash-at-point
  :if *is-mac*
  :bind (:map prog-mode-map
              ("M-s d" . dash-at-point))
  :config
  ;; specify docsets to search in different modes
  (set 'dash-at-point-mode-alist
       '((c-mode      . "c,gsl")
         (c++-mode    . "cpp,eigen,boost")
         (python-mode . "py,np,sp,plt,pd,pytorch")
         (elisp-mode  . "elisp")
         (ess-mode    . "r")
         (sh-mode     . "bash"))))
;; Integration with /Zeal/ for quick refernce (available for Linux)
(use-package zeal-at-point
  :if *is-linux*
  :bind (:map prog-mode-map
              ("M-s d" . zeal-at-point))
  :config
  (set 'dash-at-point-mode-alist
       '((c-mode      . "c,gsl,gl4")
         (c++-mode    . "c++,eigen,boost,gsl")
         (python-mode . "python 2,numpy,scipy,matplotlib,pandas")
         (ess-mode    . "r"))))

;; ----------------------------------------------
;; /gud/: debugging supports, e.g. gdb, pdb
;; ----------------------------------------------
;; usages: "M-x pdb" for Python
;;         "M-x gdb" then "M-x gdb-many-window", then "b main" "r"
;; notes: default keybindings
;; =C-c C-s= step in         =C-c C-n= next
;; =C-c C-f= step out        =C-c C-r= continue
;; =C-c C-b= set breakpoint  =C-c C-d= delete breakpoint

(use-package gud
  :bind (:map gud-mode-map
              ("<f7>"   . gud-next)   ;; setp
              ("<f8>"   . gud-step)   ;; setp in
              ("S-<f8>" . gud-finish)) ;; setp out
  :config
  (when *is-mac*
    (setq gdb-non-stop-setting nil)))

;; ----------------------------------------------
;; term/ansi-term
;; ----------------------------------------------
;; mode switch: "C-c C-j" line mode; "C-c C-k" char mode
;; send literal "C-c": "C-c C-c"
;; jump to prompts ("C-c C-p" and "C-c C-n" in line mode)
(use-package term
  :ensure nil
  :config
  (setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; ----------------------------------------------
;; Hydra support for programming (lsp, jump, rename, etc)
;; ----------------------------------------------
(define-key prog-mode-map (kbd "M-s SPC") 'hydra-coding/body)

(defvar hydra-coding--title
  (pretty-hydra-title "Programming" 'octicon "nf-oct-code"))

(pretty-hydra-define hydra-coding
  (:foreign-keys warn :title hydra-coding--title :quit-key ("q" "SPC" "C-g"))
  ("Action"
   ((";" lsp-bridge-rename "rename")
    ("s" lsp-bridge-workspace-list-symbols "symbol query")
    ("a" lsp-bridge-code-action "action")
    ("F" lsp-bridge-code-format "formatting")
    ("R" compile "run/compile"))

   "Jumping"
   (("." lsp-bridge-find-def "definition")
    ("i" lsp-bridge-find-impl "implementation")
    ("r" lsp-bridge-find-references "references")
    ("," lsp-bridge-find-def-return "return"))

   "Help"
   (("p" lsp-bridge-peek "peek")
    ("l" lsp-bridge-diagnostic-list "diagnosis")
    ("h" lsp-bridge-show-documentation "help doc")
    ("c" lsp-bridge-signature-help-fetch "call tips")
    ("d" dash-at-point "dash doc"))

   "Citre"
   (("P" citre-peek "peek")
    ("A" citre-ace-peek "ace peek")
    ("U" citre-update-this-tags-file "update")
    ("C" citre-create-tags-file "create")
    ("E" citre-edit-tags-file-recipe "recipe"))))


(provide 'init-programming)
;; ================================================
;; init-programming.el ends here
