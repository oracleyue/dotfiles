;; This file bootstraps the configuration, which is divided into a
;; number of other files.

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; load paths
(dolist (folder '("init" "site-lisp"))
  (add-to-list 'load-path (expand-file-name folder user-emacs-directory)))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file 'noerror 'nomessage)

;; packages
;; (package-initialize)           ;; required to suppress warnings
(require 'init-pkg)

;; constants
(require 'init-const)             ;; enable/disable features

;; UI (theme, modeline, dashboard, etc.)
(require 'init-ui)

;; basics
(require 'init-basics)
(require 'init-edit)

;; code completion engine
(require 'init-company)

;; completion systems (if prefer helm, load "init-helm.el")
(require 'init-ivy)

;; directory and buffer explorers
(require 'init-wm)             ;; directory explorers

;; project management
(require 'init-projectile)

;; major modes for document editing
(require 'init-text)
(require 'init-orgmode)           ;; org-mode
(require 'init-markdown)          ;; markdown
(require 'init-auctex)            ;; latex

;; additional misc mode
(require 'init-misc)

;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; Supporting tools
(require 'init-programming)          ;; edit, vc, debug, ui ...
;; For tags, see /helm-gtags/ or /counsel-gtags/ in "init-helm.el" or "init-ivy.el"

;; Language Server Protocol support
(if *use-lsp* (require 'init-lsp))

;; /Lisp/
(require 'init-lisp)

;; web development (html, css, js)
(require 'init-web)

;; /C C++/
(require 'init-cc)

;; /Python/
(require 'init-python)

;; /R/
(require 'init-r)

;; /MATLAB/
(require 'init-matlab)

;; Minority Languages
(require 'init-lang)

;; ----------------------------------------------------------------
;; Managing external applications in OS
;; ----------------------------------------------------------------

;; Calibre for ebook management
(require 'init-calibre)

;; ----------------------------------------------------------------
;; Private (You may delete the following.)
;; ----------------------------------------------------------------
(when *is-zyue* (require 'init-private))
