;; This file bootstraps the configuration, which is divided into a
;; number of other files.

;; load paths
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/init/ext")
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

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file 'noerror 'nomessage)

;; packages
;; (package-initialize)           ;; required to suppress warnings
(require 'init-pkg)

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
(require 'init-wm)             ;; directory explorers

;; major modes for document editing
(require 'init-orgmode)           ;; org-mode
(require 'init-markdown)          ;; markdown
(require 'init-auctex)            ;; latex

;; additional misc mode
(require 'init-misc)

;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; Supporting tools
(require 'init-progtools)          ;; edit, vc, debug, ui ...
;; code navigation, see /helm-gtags/ or /counsel-gtags/

;; Language Server Protocol support
(if *use-lsp* (require 'init-lsp))

;; /Lisp/
(require 'init-lisp)

;; web development (html, css, js)
(require 'init-web)

;; /C C++/
(require 'init-cc)

;; /Python/
(if *use-lsp* (require 'init-python) (require 'init-py))

;; /R/
(require 'init-r)

;; /MATLAB/
(require 'init-matlab)

;; Minority Languages
(require 'init-lang)


;; ----------------------------------------------------------------
;; Restore default configurations overwritten by other modes
;; ----------------------------------------------------------------
(require 'init-restore)
