;; ================================================================
;; Basic Configrations
;; ================================================================
;; Last modified on 15 Sep 2017


;; fix $PATH for emacs in Mac OS X
(require 'exec-path-from-shell)
(push "PYTHONPATH" exec-path-from-shell-variables)
(exec-path-from-shell-initialize)

;; more environment variables
;; (setenv "MATLAB_JAVA" "/usr/lib/jvm/java-7-openjdk/jre")

;; cursors
(setq-default cursor-type 'box)  ;"bar", "box" (default)
(blink-cursor-mode -1)  ;stop cursor blinking

;; font size adjustment
;; use C-x C-0 first, then use +/- to tune the size.

;; disable backup file creation
(setq backup-inhibited t)

;; answer with y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; control the width of a literal tab (C-q TAB; key=9)
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
;;   (setq mac-command-modifier 'control)  ; use Command key also as Control
;;   (setq mac-option-modifier 'meta))  ; NOT need

;; startup Dired folders
(defun y:dired-open-folders-startup ()
  (interactive)
  "Setup the startup folders. Used in .emacs"
  (cond ((string-equal system-type "darwin")
         (dired (expand-file-name "~/Public/Dropbox/Academia/Seminars"))
         ;(dired (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote"))
         (find-file (expand-file-name "~/Public/Dropbox/Academia/ToDoList.org"))
         (find-file (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote/PhD.org")))
        ((string-equal system-type "gnu/linux")
         (dired (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote"))
         (dired (expand-file-name "~/Workspace"))))
  (switch-to-buffer "*scratch*"))

;; quick start email editing
(defun draft-email ()
  (interactive)
  (find-file (expand-file-name "~/Documents/email.tmp.md"))
  (auto-fill-mode 1)
  (set-fill-column 70))

;; supports for Chinese
;; setting font set
(if(display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "WenQuanYi Micro Hei"))))
;; stop cursor blinking at the first letter when using pinyin or wubi
(setq redisplay-dont-pause nil)