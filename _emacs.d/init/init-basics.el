;; ================================================================
;; Basic Configrations
;; ================================================================
;; Last modified on 20 Feb 2020


;; ----------------------------------------------
;; Basics
;; ----------------------------------------------
(tool-bar-mode -1)                  ;; remove toolbar
(scroll-bar-mode -1)                ;; remove scrolling bar
(when (and *is-linux* (string= linux-desktop-env "i3"))
           (menu-bar-mode -1))     ;; remove menus in i3
(when *is-terminal*
  (setq inhibit-startup-screen t))  ;; remove startup buffer
(column-number-mode t)              ;; show column number
(fset 'yes-or-no-p 'y-or-n-p)       ;; use y-n as yes-no
(setq backup-inhibited t)           ;; stop backup
;; (setq-default case-fold-search nil) ;; use case-sensitive search

;; encodings (utf-8 for everything)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; display “lambda” as “λ”
(global-prettify-symbols-mode t)  ;; it also prettify Auctex, raise issues

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; cursors
(setq-default cursor-type 'bar) ; "bar", "box" (default)
(blink-cursor-mode t)           ; -1 stops cursor blinking

;; daemons and clients
;;   - "main"    for general purpose (light-theme, startup folders)
;;   - "coding"  for coding (dark-theme, startup folders)
;; start emacs server: (server-start)
;; start daemon with name: "emacs --daemon=main"
;; connect to server: "emacsclient -nc --socket-name=main"
;; if using tcp: "emacsclient -nc --server-file=main"
;; (setq-default server-use-tcp t)  ;; nil to use local socket instead tcp
;; Note:
;; - If using tcp, starting daemon creates a server file under "~/.emacs.d/server/";
;; - If you kill emacs daemon process directly by system command "kill", the server file remains there, which will stop the next start of emacs daemon.
;; - The right way to kill the server is using ~kill-emacs~, which may be setup as =emacsclient -nc --server-file=main --eval "(kill-emacs)"=, which automatically delete the server file.

;; fix PATH for emacs in Mac OS X
(use-package exec-path-from-shell
  :demand
  :config
  (push "LC_ALL" exec-path-from-shell-variables)
  (push "LANG" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))

;; fill-column
(defconst *fill-column-sans* 90)
(defconst *fill-column-mono* 72)
(setq-default fill-column *fill-column-mono*)

;; file should end with a newline
(setq-default mode-require-final-newline t)

;; font size adjustment
;; use C-x C-0 first, then use +/- to tune the size.

;; TAB
(setq-default indent-tabs-mode nil
              tab-stop-list ()
              tab-width 4)
;; insert TAB: =C-q TAB= (=TAB= trigger complete or insert 4 spaces)
;; insert 4-spaced TAB: =tab-to-tab-stop= or "M-i"
;; converting TAB and 4-spaces of regions: =tabify= and =untabify=

;; enable clipboard in emacs  (only need for emacs in terminal)
;; (setq x-select-enable-clipboard t)
;; paste from PRIMARY (same as middle mouse click)
(defun paste-primary-selection ()
  (interactive)
  (insert (x-get-selection 'PRIMARY)))
(global-set-key (kbd "S-<insert>") 'paste-primary-selection)

;; <TAB> key
(setq tab-always-indent 'complete)  ;; use TAB to indent or complete

;; configure mark-ring
(setq set-mark-command-repeat-pop nil)
(setq mark-ring-max 16)
(setq global-mark-ring-max 32)

;; recent files
(setq recentf-max-saved-items 100)
;; clean non-exist recent files: "M-x recentf-cleanup"
;; remove manually: "M-x recentf-edit-list"

;; srcolling control (move 3 lines each time)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)

;; suppress redefinition warnings
(setq ad-redefinition-action 'accept)

;; increase memeory for emacs to avoid garbage collections slow it down
(setq gc-cons-threshold (* 20 1024 1024))   ; 20MB, default <0.8MB

;; register
;; "C-x r SPC": add point to register
;; "C-x r j"  : jump to registered point
;; ivy list support: "M-g r"
(setq register-preview-delay 0) ; no delay

;; bookmarks
;; "C-x r b": create or jump to bookmarks
;; "C-x r l": list and manage bookmarks
;; ivy list support: "M-g b"
(setq bookmark-save-flag 1)  ;; auto-save it whenever changed

;; text scale amount (=C-x C-0=)
(setq text-scale-mode-step 1.05)

;; use variable-width font types in text-mode
;; (defun y-variable-width-text-mode ()
;;   (interactive)
;;   (variable-pitch-mode t))
;; (add-hook 'text-mode-hook 'y-variable-width-text-mode)

;; keymap modification for OS X
(when (and (string-equal system-type "darwin") nil)
  (setq mac-command-modifier 'control)  ; use command as control
  (setq mac-control-modifier 'super))   ; use control as super
;; keymap modification for Linux
(when (and (string-equal system-type "gnu/linux") t)
  (setq x-ctrl-keysym 'super)
  (setq x-meta-keysym 'ctrl)
  (setq x-super-keysym 'meta))

;; unset keys
(global-unset-key (kbd "s-k"))  ;; =super-k= kill current buffer

;; /repeat-mode/ to derease key stroke
;; e.g., "undo" via "C-x u" can be done by "C-x u u u" for 3 undo's
(use-package repeat
  :disabled     ;; bug: pause daemon
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom (repeat-exit-key (kbd "RET")))

;; show size of files (in modeline)
(size-indication-mode t)

;; line moving (bug on Mac: moving jaggly sometime)
(setq line-move-visual nil)

;; quick start email editing
(defun zyue/draft-email ()
  (interactive)
  (find-file (expand-file-name "~/Documents/.email.tmp.md"))
  (auto-fill-mode -1)
  (setq-local fill-column *fill-column-mono*))

;; quick draft formulas in LaTeX
(defun zyue/draft-formula ()
  (interactive)
  (split-window nil -8 'below)
  (other-window 1)
  (find-file (expand-file-name "~/Documents/.formula.tex"))
  (with-current-buffer ".formula.tex"
    (LaTeX-mode)))

;; stop cursor blinking bug when typing Chinese/Japanese on OS X
;(setq redisplay-dont-pause nil)

;; proxy (allowing github supports)
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))

;; ----------------------------------------------
;; /diminish/: remove minor mode names from modeline
;; ----------------------------------------------
(use-package diminish
  :demand
  :config
  ;; "ARev" keyword
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode))
  ;; "Abbrev"keyword
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))
  ;; line wrapping
  (eval-after-load "simple"
    '(progn
       (diminish 'visual-line-mode)
       (diminish 'auto-fill-function))))

;; ----------------------------------------------
;; /saveplace/ (built-in): restore cursor position when reopen files
;; ----------------------------------------------
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; ----------------------------------------------
;; auto-save files when stop editing
;; ----------------------------------------------
;; (require 'auto-save)
;; (setq auto-save-slient t)
;; (auto-save-enable)

;; ----------------------------------------------
;; /whitespace/ (built-in): show whitespaces, like space, \t, \v
;; ----------------------------------------------
(use-package whitespace
  :ensure nil
  :diminish
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  ;; show trailing whitespaces at lines
  (setq whitespace-style '(face tabs trailing)))

;; ----------------------------------------------
;; /hl-line/ (built-in): highlight current line
;; ----------------------------------------------
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; ----------------------------------------------
;; /beacon/: a colorful bar trailing to highlight cursor
;; ----------------------------------------------
(use-package beacon
  :demand
  :diminish
  :config (beacon-mode 1))

;; ----------------------------------------------
;; /smartparens/: insert pairs of parenthesis/brackets
;; ----------------------------------------------
(use-package smartparens
  :demand
  :diminish
  :bind (:map smartparens-mode-map
              ;; nativation
              ("C-M-f"   . sp-forward-sexp)
              ("C-M-b"   . sp-backward-sexp)
              ("C-M-d"   . sp-down-sexp)
              ("C-M-u"   . sp-up-sexp)
              ("C-M-n"   . sp-next-sexp)
              ("C-M-p"   . sp-previous-sexp)
              ("C-S-a"   . sp-beginning-of-sexp)
              ("C-S-e"   . sp-end-of-sexp)
              ;; mark
              ("C-M-SPC" . sp-mark-sexp)
              ;; warp, unwrap and rewrap
              ("C-M-s"   . sp-splice-sexp)
              ("C-M-w"   . sp-rewrap-sexp)
              ;; kill, copy
              ("C-M-k"   . sp-kill-sexp)
              ;; expand and contract
              ("C-<right>"    . sp-forward-slurp-sexp)
              ("C-<left>"     . sp-forward-barf-sexp)
              ("C-M-<left>"   . sp-forward-slurp-sexp)
              ("C-M-<right>"  . sp-forward-barf-sexp)
              ;; split, join and raise
              ("C-M-t"   . sp-split-sexp)
              ("C-M-j"   . sp-join-sexp)
              ("C-M-r"   . sp-raise-sexp))
  :config
  (require 'smartparens-config) ;; default config
  (smartparens-global-mode 1)
  ;; highlight pairs (e.g. brackets)
  ;; (show-smartparens-global-mode 1)
  (with-eval-after-load 'smartparens  ;; use default /show-paren/
    (show-smartparens-global-mode -1)
    (show-paren-mode t))  ;; fixes huge delay when hit Enter after "do" in bash "FOR" loop
  ;; disable highlights between pairs firstly inserted
  (setq sp-highlight-pair-overlay nil)
  ;; disable smartparens for specific modes
  ;; (add-to-list 'sp-ignore-modes-list 'latex-mode)
  )

;; ----------------------------------------------
;; /bash-completion/: TAB complete alias and functions
;; ----------------------------------------------
(use-package bash-completion
  :config (bash-completion-setup))

;; ------------------------------------------------
;; /tramp/: manage ssh and remote access
;; ------------------------------------------------
(setq tramp-default-method "ssh")
;; usages:
;; - "C-x C-f /ssh:gaia:/home/users/zuogong.yue/..." or without "ssh:"
;; - "C-x C-f /sudo::/etc/hosts"

;; ------------------------------------------------
;; Integrate /dabbrev/ into completion-at-point (Capf)
;; ------------------------------------------------
(use-package dabbrev
  :ensure nil
  ;; swap M-/ and C-M-/
  :bind (("M-/"   . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  ;; ignore buffers/modes
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  ;; inferface to add dabbrev into capf
  (defun dabbrev-complation-at-point ()
    "User-defined dabbrev function for `completetion-at-point-functions'."
    (dabbrev--reset-global-variables)
    (let* ((abbrev (dabbrev--abbrev-at-point))
           (candidates (dabbrev--find-all-expansions abbrev t))
           (bnd (bounds-of-thing-at-point 'symbol)))
      (list (car bnd) (cdr bnd) candidates))))


(provide 'init-basics)
;; ================================================
;; init-basics.el ends here
