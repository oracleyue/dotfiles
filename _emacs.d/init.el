;; This bootstraps user-defined configurations, which are provided by
;; files in "init/".

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; load paths
(dolist (folder '("init" "site-lisp"))
  (add-to-list 'load-path (expand-file-name folder user-emacs-directory)))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file 'noerror 'nomessage)

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
(when *use-company* (require 'init-company))
(require 'init-snippets)

;; global completion systems
(require 'init-ivy)

;; directory and buffer explorers
(require 'init-windows)

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
(require 'init-programming)           ;; edit, vc, debug, ui ...

;; LSP
(pcase *lsp-client*
  ('lsp-mode   (require 'init-lsp))          ;; lsp-mode and dap-mode
  ('lsp-bridge (require 'init-lsp-bridge)))  ;; lsp-bridge mode (fastest)

;; /Lisp/
(require 'init-lisp)

;; web development (/html, css, js/)
(require 'init-web)

;; /C C++/
(require 'init-cc)

;; /Python/
(require 'init-python)

;; /R/
(require 'init-r)

;; /MATLAB/
(require 'init-octave)

;; minority languages
(require 'init-lang)

;; ----------------------------------------------------------------
;; Utilities and External App Calls
;; ----------------------------------------------------------------
(require 'init-utils)

;; ----------------------------------------------------------------
;; Hydra supports (be careful on dependencies)
;; ----------------------------------------------------------------
;; Window operations via Hydra + Ace-window
(when *use-hydra* (require 'init-hydra-aw))
(when *use-hydra* (require 'init-hydra-coding))

;; ----------------------------------------------------------------
;; Private (You may delete the following.)
;; ----------------------------------------------------------------
(when *is-zyue* (require 'init-private))
