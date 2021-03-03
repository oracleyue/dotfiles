;; This bootstraps user-defined configurations, which are provided by
;; files in "init/".

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; load paths
(dolist (folder '("init" "site-lisp"))
  (add-to-list 'load-path (expand-file-name folder user-emacs-directory)))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file 'noerror 'nomessage)

;; packages
;; (package-initialize)           ;; required to suppress warnings
(require 'init-pkg)

;; ----------------------------------------------------------------
;; Basics, Completion Systems and Window/Buffer Management
;; ----------------------------------------------------------------
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
(require 'init-windows)

;; remote access (ssh, etc.)
(require 'init-remote)

;; ----------------------------------------------------------------
;; Text Editing (org, markdown, latex, blog)
;; ----------------------------------------------------------------
(require 'init-text)              ;; generic text

(require 'init-orgmode)           ;; org-mode
(require 'init-markdown)          ;; markdown
(require 'init-auctex)            ;; latex

(require 'init-blog)              ;; blog

;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; project management
(require 'init-projectile)

;; essential programming supports
(require 'init-programming)          ;; edit, vc, debug, ui ...

;; LSP
(require 'init-lsp)

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

;; minority languages
(require 'init-lang)

;; ----------------------------------------------------------------
;; External applications in OS
;; ----------------------------------------------------------------
(require 'init-external)

;; ----------------------------------------------------------------
;; Private (You may delete the following.)
;; ----------------------------------------------------------------
(when *is-zyue* (require 'init-private))
