;; ================================================================
;; Basic Configrations
;; ================================================================
;; Last modified on 15 Sep 2017

;; Install required Emacs packages
(setq custom/basic-packages
      '(smartparens
        bash-completion
        exec-path-from-shell))
(custom/install-packages custom/basic-packages)


;; basics
(setq inhibit-startup-screen t)
(column-number-mode t)
(setq backup-inhibited t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq-default case-fold-search nil)  ;; case-sensitive search
(when (string= linux-desktop-env "i3")
  (menu-bar-mode -1))

;; encodings
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; daemons and clients
;;   - "main"    for general purpose (light-theme, startup folders)
;;   - "coding"  for coding (dark-theme, startup folders)
;;   - "ac-mode" for ac-complete (the rest as "coding")
;; start emacs server (use =emacsclient -a "" -c= anywhere else)
;; (server-start)
;; alternatively, use systemd to start "emacs --daemon" on startup (better!)
;; start daemon with name: "emacs --daemon=main"
;; connect to server: "emacsclient -nc --socket-name=main"
;; if using tcp: "emacsclient -nc --server-file=main"
;; (setq-default server-use-tcp t)  ;; nil to use local socket instead tcp
;; Note:
;; - If using tcp, starting daemon creates a server file under "~/.emacs.d/server/";
;; - If you kill emacs daemon process directly by system command "kill", the server file remains there, which will stop the next start of emacs daemon.
;; - The right way to kill the server is using ~kill-emacs~, which may be setup as =emacsclient -nc --server-file=main --eval "(kill-emacs)"=, which automatically delete the server file.

;; fill-column
(defconst *fill-column-sans* 90)
(defconst *fill-column-mono* 75)
(setq-default fill-column *fill-column-mono*)

;; fix PATH for emacs in Mac OS X
(require 'exec-path-from-shell)
(push "LC_ALL" exec-path-from-shell-variables)
(push "LANG" exec-path-from-shell-variables)
;; (push "PYTHONPATH" exec-path-from-shell-variables)
(push "WM" exec-path-from-shell-variables)
(exec-path-from-shell-initialize)

;; cursors
(when (string-equal system-type "darwin")
  (setq-default cursor-type 'bar)) ; "bar", "box" (default)
(blink-cursor-mode t)  ; -1 stops cursor blinking

;; font size adjustment
;; use C-x C-0 first, then use +/- to tune the size.

;; TAB
(setq-default indent-tabs-mode nil
              tab-stop-list ()
              tab-width 4)

;; enable clipboard in emacs  (only need for emacs in terminal)
;; (setq x-select-enable-clipboard t)

;; configure mark-ring
(setq set-mark-command-repeat-pop nil)
(setq mark-ring-max 16)
(setq global-mark-ring-max 32)

;; srcolling control (move 3 lines each time)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)

;; suppress redefinition warnings
(setq ad-redefinition-action 'accept)

;; increase memeory for emacs to avoid garbage collections slow it down
(setq gc-cons-threshold (* 20 1024 1024))   ; 20MB, default <0.8MB

;; text scale amount (=C-x C-0=)
(setq text-scale-mode-step 1.05)

;; use variable-width font types in text-mode
;; (defun y-variable-width-text-mode ()
;;   (interactive)
;;   (variable-pitch-mode t))
;; (add-hook 'text-mode-hook 'y-variable-width-text-mode)

;; keymap modification for OS X
(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'control)  ; use command as control
  (setq mac-control-modifier 'super))   ; use control as super

;; show size of files (in modeline)
(size-indication-mode t)

;; line moving (bug on Mac: moving jaggly sometime)
(setq line-move-visual nil)

;; default browser
(if (string-equal system-type "darwin")
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program
          (expand-file-name "~/bin/web-browser")) ;use Safari
  (setq browse-url-browser-function 'browse-url-firefox))

;; startup Dired folders
(defun zyue/dired-open-folders-startup ()
  (interactive)
  "Setup the startup folders. Used in .emacs"
  (dired (expand-file-name "~/Public/Dropbox/Academia/Seminars"))
  (find-file (expand-file-name "~/Public/Dropbox/Academia/ToDoList.org"))
  (find-file (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote/Research.org"))
  (switch-to-buffer "*scratch*"))

;; quick start email editing
(defun draft-email ()
  (interactive)
  (find-file (expand-file-name "~/Documents/.email.tmp.md"))
  (auto-fill-mode 1)
  (setq-local fill-column *fill-column-mono*))

;; quick draft formulas in LaTeX
(defun draft-formula ()
  (interactive)
  (split-window nil -8 'below)
  (other-window 1)
  (find-file (expand-file-name "~/Documents/.formula.tex"))
  (with-current-buffer ".formula.tex"
    (LaTeX-mode)))

;; supports for Chinese (moved to init-ui.el)
;; setting font set
;; (if (display-graphic-p)
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset
;;                         (font-spec :family "WenQuanYi Micro Hei"))))
;; stop cursor blinking bug when using PinYin on OS X
(setq redisplay-dont-pause nil)

;; ----------------------------------------------
;; auto-save files when stop editing
;; ----------------------------------------------
;; (require 'auto-save)
;; (auto-save-enable)
;; (setq auto-save-slient t)

;; ----------------------------------------------
;; /hl-sexp/: matching a pair of braces and hightlight the contents
;; ----------------------------------------------
;; (require 'hl-sexp)
;; (add-hook 'lisp-mode-hook 'hl-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; ----------------------------------------------
;; /smartparens/: insert pairs of parenthesis/brackets
;; ----------------------------------------------
(use-package smartparens
  :defer nil
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
(require 'bash-completion)
(bash-completion-setup)



(provide 'init-basics)
;; ================================================
;; init-basics.el ends here
