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
(setq-default fill-column 80)
;; (setq-default case-fold-search nil)  ;; case-sensitive search

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

;; fix PATH for emacs in Mac OS X
(require 'exec-path-from-shell)
(push "LC_ALL" exec-path-from-shell-variables)
(push "LANG" exec-path-from-shell-variables)
;; (push "PYTHONPATH" exec-path-from-shell-variables)
(exec-path-from-shell-initialize)

;; cursors
(setq-default cursor-type 'box)  ;"bar", "box" (default)
(when (or *is-server-main* *is-app* *is-server-plain*)
  (setq-default cursor-type 'bar))
(blink-cursor-mode t)  ;-1 stops cursor blinking

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

;; auto-save files when stop editing
;; (require 'auto-save)
;; (auto-save-enable)
;; (setq auto-save-slient t)

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
  (cond ((string-equal system-type "darwin")
         (dired (expand-file-name "~/Public/Dropbox/Academia/Seminars"))
         (find-file (expand-file-name "~/Public/Dropbox/Academia/ToDoList.org"))
         (find-file (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote/Research.org")))
        ((string-equal system-type "gnu/linux")
         (dired (expand-file-name "~/Public/Dropbox/oracleyue/OrgNote"))
         (dired (expand-file-name "~/Workspace"))))
  (switch-to-buffer "*scratch*"))

;; quick start email editing
(defun draft-email ()
  (interactive)
  (find-file (expand-file-name "~/Documents/.email.tmp.md"))
  (auto-fill-mode 1)
  (setq-local fill-column 75))

;; quick draft formulas in LaTeX
(defun draft-formula ()
  (interactive)
  (split-window nil -8 'below)
  (other-window 1)
  (find-file (expand-file-name "~/Documents/.formula.tex")))

;; supports for Chinese (moved to init-theme.el)
;; setting font set
;; (if (display-graphic-p)
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset
;;                         (font-spec :family "WenQuanYi Micro Hei"))))
;; stop cursor blinking bug when using PinYin on OS X
(setq redisplay-dont-pause nil)

;; /hl-sexp/: matching a pair of braces and hightlight the contents
;; (require 'hl-sexp)
;; (add-hook 'lisp-mode-hook 'hl-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; /smartparens/: insert pairs of parenthesis/brackets
(use-package smartparens
  :ensure t
  :defer nil
  :bind (:map smartparens-mode-map
              ;; nativation
              ("C-M-f"   . sp-forward-sexp)
              ("C-M-b"   . sp-backward-sexp)
              ("C-M-d"   . sp-down-sexp)
              ("C-M-u"   . sp-up-sexp)
              ("C-M-a"   . sp-beginning-of-sexp)
              ("C-M-e"   . sp-end-of-sexp)
              ;; mark
              ("C-M-SPC" . sp-mark-sexp)
              ;; warp, unwrap and rewrap
              ("C-M-s"   . sp-splice-sexp)
              ("C-M-w"   . sp-rewrap-sexp)
              ;; kill, copy
              ("C-M-k"   . sp-kill-sexp)
              ;; expand and contract
              ("C-M-p"   . sp-forward-barf-sexp)
              ("C-M-c"   . sp-forward-slurp-sexp)
              ;; split, join and raise
              ("C-M-t"   . sp-split-sexp)
              ("C-M-j"   . sp-join-sexp)
              ("C-M-r"   . sp-raise-sexp))
  :config
  (require 'smartparens-config) ;; default config
  (smartparens-global-mode 1)
  ;; highlight pairs (e.g. brackets)
  (show-smartparens-global-mode 1) ;; replace default /show-paren/
  ;; disable highlights between pairs firstly inserted
  (setq sp-highlight-pair-overlay nil))

;; /bash-completion/: TAB complete alias and functions
(require 'bash-completion)
(bash-completion-setup)

;; extracting .el from .org config files
(require 'ob-tangle)
(defun y/init-el ()
  "Use org-babel to extract elisp code blocks from all .org files
in ~/.emacs.d/init/ and export them using the same file names."
  (interactive)
  (setq prepath (concat user-emacs-directory "init/"))
  (let ((files (directory-files prepath nil "\\.org$")))
    (dolist (file files)
      (setq file-dest (concat prepath
                              (file-name-sans-extension file) ".el"))
      (setq file (concat  prepath file))
      (org-babel-tangle-file file file-dest "emacs-lisp"))))

(defun y/init-el-current ()
  "Use org-babel to extract elisp code blocks from the current .org
file and export into ~/.emacs.d/init/ with the same file name."
  (interactive)
  (setq outpath (concat user-emacs-directory "init/"))
  (let ((file (buffer-file-name)))
    (if (not (string-equal (file-name-extension file) "org"))
        (user-error "Error: not an org-mode file")
      (setq file-dest (concat outpath
                              (file-name-base file) ".el"))
      (org-babel-tangle-file file file-dest "emacs-lisp"))))



(provide 'init-basics)
;; ================================================
;; init-basics.el ends here
