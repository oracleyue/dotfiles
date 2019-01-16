;; This file bootstraps the configuration, which is divided into a
;; number of other files.

;; load paths
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/init/unmaintained")
(add-to-list 'load-path "~/.emacs.d/git")

;; constants
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-terminal* (not (or (display-graphic-p) (daemonp))))
(defconst *is-app* (and (display-graphic-p) (not (daemonp))))
(defconst *is-server-plain* (eq t (daemonp)))
(defconst *is-server-main* (string-equal "main" (daemonp)))
(defconst *is-server-coding* (string-equal "coding" (daemonp)))
(defconst *is-server-ac* (string-equal "ac-mode" (daemonp)))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file 'noerror 'nomessage)

;; packages
;; (package-initialize)           ;; required to suppress warnings
(require 'init-packages)

;; feature control
(require 'init-features)          ;; enable/disable features

;; UI (theme, modeline, etc.)
(require 'init-ui)

;; basics
(require 'init-basics)
(require 'init-edit)
;; (require 'init-evil)           ;; use vim in emacs

;; code completion engine (/company/ or /auto-complete/)
(if *use-company* (require 'init-company) (require 'init-ac))

;; completion systems
(if *use-helm* (require 'init-helm) (require 'init-ivy))

;; directory and buffer explorers
(require 'init-dired)             ;; directory explorers

;; major modes for document editing
(require 'init-orgmode)           ;; org-mode
(require 'init-markdown)          ;; markdown
(require 'init-auctex)            ;; latex

;; additional misc mode
(require 'init-misc)

;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; general programming supports
(require 'init-progtools)          ;; edit, vc, debug, ui ...
;; code navigation, see /helm-gtags/ or /counsel-gtags/

;; programming environment for /Lisp/
(require 'init-lisp)

;; programming environment for /HTML, js/
(require 'init-web)

;; programming environment for /C C++/ and /Python/
(if (and *use-lsp* *use-company*)
    (require 'init-lsp)
  ;; C/C++
  (if *use-company* (require 'init-cc) (require 'init-cc-ac))
  ;; Python
  (if *use-company* (require 'init-py) (require 'init-py-ac))
  )

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
