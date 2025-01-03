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

;; keybinding cheetsheet
(require 'init-helper)

;; snippets
(require 'init-snippets)

;; global completion systems
(pcase *ac-system*
  ('ivy (require 'init-ivy))
  ('vertico (require 'init-vertico)))

;; code completion
(pcase *ac-engine*
  ('company (require 'init-company))
  ('corfu   (require 'init-corfu)))

;; directory and buffer explorers
(require 'init-windows)
(require 'init-treemacs)

;; startup screen (dashboard)
(require 'init-dashboard)

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

;; LSP
(pcase *lsp-client*
  ('lsp-mode   (require 'init-lsp))          ;; lsp-mode and dap-mode
  ('lsp-bridge (require 'init-lsp-bridge))   ;; lsp-bridge mode (fastest)
  ('eglot      (require 'init-eglot))        ;; eglot
  ('nil nil))  ;; no load

;; DAP
(require 'init-dap)

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

;; additional programming supports
(require 'init-programming)           ;; edit, vc, debug, ui ...

;; ----------------------------------------------------------------
;; Utilities and External App Calls
;; ----------------------------------------------------------------
(require 'init-utils)

;; ----------------------------------------------------------------
;; Private (You may delete the following.)
;; ----------------------------------------------------------------
(when *is-zyue* (require 'init-private))
