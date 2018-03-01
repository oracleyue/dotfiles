(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; Your init file should contain only one such instance.
 '(default ((t (:font "DejaVu Sans Mono-15"))))  ;; 13/15(mac), 10.5/12(linux)
 '(fixed-pitch ((t (:family "Roboto Mono"))))
 '(variable-pitch ((t (:family "Roboto")))))

;; stop emacs automatically editing .emacs
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; load paths
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init/unmaintained" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "git" user-emacs-directory))

;; constants
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-server-main* (string-equal "main" (daemonp)))
(defconst *is-server-coding* (string-equal "coding" (daemonp)))
(defconst *is-server-ac* (string-equal "ac-mode" (daemonp)))

;; packages
;; (package-initialize)
(require 'init-packages)

;; theme
(require 'init-theme)

;; basics
(require 'init-basics)
(require 'init-edit)
;; (require 'init-evil)           ;; use vim in emacs

;; feature control
(require 'init-features)          ;; enable/disable features

;; code completion engine (/company/ or /auto-complete/)
(if *use-company* (require 'init-company) (require 'init-ac))

;; helm and extensions
(require 'init-helm)
(require 'init-helm-swoop)
(require 'init-helm-proj)         ;; project management

;; directory and buffer explorers
(require 'init-dired)             ;; directory explorers

;; major modes for document editing
(require 'init-orgmode)           ;; org-mode
(require 'init-markdown)          ;; markdown
(require 'init-auctex)            ;; latex


;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; general programming supports
(require 'init-progsupp)          ;; edit, vc, debug, ui ...
(require 'init-gtags)             ;; code navigation

;; programming environment for /Lisp/
(require 'init-lisp)

;; programming environment for /HTML, js/
(require 'init-web)

;; programming environment for /C C++/
(if *use-company* (require 'init-cc) (require 'init-cc-ac))

;; programming environment for /Python/
(if *use-company* (require 'init-py) (require 'init-py-ac))

;; programming environment for /R/
(if *use-company* (require 'init-r) (require 'init-r-ac))

;; programming environment for /MATLAB/
(require 'init-matlab)

;; major-modes for minority languages
(require 'init-proglang)


;; ----------------------------------------------------------------
;; Restore default configurations overwritten by other modes
;; ----------------------------------------------------------------
(require 'init-restore)
