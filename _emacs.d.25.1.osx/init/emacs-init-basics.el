;; ================================================================
;; Basic Configrations
;; ================================================================
;; Last modified on 15 Sep 2017

;; basics
(setq inhibit-startup-screen t)
(column-number-mode t)
(setq backup-inhibited t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; encodings
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; servers
;; profiles
;;   - "main"    for general purpose (light-theme, startup folders)
;;   - "coding"  for coding (dark-theme, startup folders)
;;   - "ac-mode" for ac-complete (the rest as "coding")
;; start emacs server (use =emacsclient -a "" -c= anywhere else)
;; (server-start)
;; alternatively, use systemd to start "emacs --daemon" on startup (better!)
(setq server-use-tcp t)

;; fix PATH for emacs in Mac OS X
(require 'exec-path-from-shell)
(push "LC_ALL" exec-path-from-shell-variables)
(push "LANG" exec-path-from-shell-variables)
;; (push "PYTHONPATH" exec-path-from-shell-variables)
(exec-path-from-shell-initialize)

;; user-defined environment variables
;; (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages/:${PYTHONPATH}")

;; cursors
(setq-default cursor-type 'box)  ;"bar", "box" (default)
(blink-cursor-mode t)  ;-1 stops cursor blinking

;; font size adjustment
;; use C-x C-0 first, then use +/- to tune the size.

;; configure TAB
(setq tab-stop-list
      (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
;; the width of a literal tab (C-q TAB; key=9)
(setq-default tab-width 4)
;; use spaces instead of evil tabs, width controled by "tab-stop-list"
(setq-default indent-tabs-mode nil)

;; enable clipboard in emacs  (only need for emacs in terminal)
;; (setq x-select-enable-clipboard t)

;; configure mark-ring
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 16)
(setq global-mark-ring-max 32)

;; mouse control
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))  ;2 line at a time
(setq mouse-wheel-progressive-speed nil)

;; suppress redefinition warnings
(setq ad-redefinition-action 'accept)

;; increase memeory for emacs to avoid garbage collections slow it down
(setq gc-cons-threshold (* 20 1024 1024))   ; 20MB, default <0.8MB

;; use variable-width font types in text-mode
;; (defun y-variable-width-text-mode ()
;;   (interactive)
;;   (variable-pitch-mode t)
;;   (text-scale-increase 0.5))
;; (add-hook 'text-mode-hook 'y-variable-width-text-mode)

;; use command as control in OS X for emacs
;; (when (string-equal system-type "darwin")
;;   (setq mac-command-modifier 'control)  ; use command as control
;;   (setq mac-option-modifier 'meta))     ; may not need

;; default browser
(if (string-equal system-type "darwin")
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program (expand-file-name "~/bin/web-browser")) ;use Safari
  (setq browse-url-browser-function 'browse-url-firefox))

;; startup Dired folders
(defun y:dired-open-folders-startup ()
  (interactive)
  "Setup the startup folders. Used in .emacs"
  (cond ((string-equal system-type "darwin")
         (dired (expand-file-name "~/Public/Dropbox/Academia/Seminars"))
         (dired (expand-file-name
                 "~/Public/Dropbox/oracleyue/blog/oracleyue.github.io/source/_posts"))
         (find-file (expand-file-name "~/Public/Dropbox/Academia/ToDoList.org"))
         (find-file (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote/PhD.org")))
        ((string-equal system-type "gnu/linux")
         (dired (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote"))
         (dired (expand-file-name "~/Workspace"))))
  (switch-to-buffer "*scratch*"))

;; quick start email editing
(defun draft-email ()
  (interactive)
  (find-file (expand-file-name "~/Documents/.email.tmp.md"))
  (auto-fill-mode 1)
  (set-fill-column 75))

;; supports for Chinese
;; setting font set
(if(display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "WenQuanYi Micro Hei"))))
;; stop cursor blinking at the first letter when using pinyin or wubi
(setq redisplay-dont-pause nil)

;; /hl-sexp/: matching a pair of braces and hightlight the contents
;; (require 'hl-sexp)
;; (add-hook 'lisp-mode-hook 'hl-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; highlight brackets
(show-paren-mode t)
;; /smartparens/: insert pairs of parenthesis/brackets
(require 'smartparens-config)
(smartparens-global-mode 1)
;; (show-smartparens-global-mode 1) ;; use show-paren-mode

;; /bash-completion/: TAB complete alias and functions
(require 'bash-completion)
(bash-completion-setup)



(provide 'emacs-init-basics)
;; ================================================
;; emacs-init-basics.el ends here