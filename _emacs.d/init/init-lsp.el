;; ================================================================
;; Emacs client for the Language Server Protocol
;; ================================================================
;; Last modified on 24 Feb 2020

;; System Dependencies:
;; - install LSP language servers
;;   - C++: clangd from "brew install llvm"
;;   - Python: pyls from "pip install python-language-server"
;;             mspyls from vscode (see "init-python.el")
;; - install DAP language servers
;;   - C++: lldb-vscode from "brew install llvm"
;;   - Python: "pip install ptvsd"

;; Warning: you have to keep "dash" and "company" modes update-to-date
;; whenever update lsp packages.

;; Features:
;; LSP mode: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off
;; Dap mode: https://emacs-lsp.github.io/dap-mode

;; Usages:
;; - use "projectile" to start a workspace or use "lsp-workspace-folders-add".
;; - use "lsp-describe-sessions" to check status


;; ----------------------------------------------------------------
;; lsp-mode as emacs LSP client
;; ----------------------------------------------------------------
(use-package lsp-mode
  :demand
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("s-l l"   . lsp-ui-flycheck-list))
  :init
  (setq lsp-keymap-prefix "s-l")               ; set prefix for lsp-command-keymap
  (setq lsp-auto-guess-root     nil            ; detect project root
        lsp-completion-provider :none          ; diable add company-capf, set later
        read-process-output-max (* 1024 1024)) ; accelearate string concat in elisp

  :config
  ;; set syntax checker
  (setq lsp-diagnostics-provider :flycheck)

  ;; call signature help
  (setq lsp-signature-render-documentation nil)  ;; keep tip, hide doc
  (setq lsp-signature-doc-lines 3)

  ;; eldoc in modeline
  (setq lsp-eldoc-enable-hover nil)

  ;; modeline diagnostics
  (setq lsp-modeline-diagnostics-enable t)

  ;; disable uncessary features for better performance
  (setq lsp-enable-folding             nil
        lsp-enable-snippet             nil
        lsp-enable-links               nil)
  (setq lsp-log-io nil))   ; enable log only for debug

(use-package lsp-ui
  :after lsp-mode
  ;; :custom-face (lsp-ui-doc-background ((t (:background nil))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)
              ("M-RET" . lsp-ui-doc-focus-frame))
  :config
  ;; configure lsp features
  (setq ;; lsp-ui-doc
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top   ;; at-point
        lsp-ui-doc-max-height 15
        ;; lsp-ui-imenu
        lsp-ui-imenu-auto-refresh t
        ;; lsp-ui-sideline
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t)
  ;; enable lsp-ui components
  (setq lsp-ui-doc-enable      t
        lsp-ui-imenu-enable    t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable     nil))


;; ----------------------------------------------------------------
;; Debugging via Debug Adapter Protocol (DAP)
;; ----------------------------------------------------------------
(use-package dap-mode
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
              ("<f5>"    . dap-debug)
              ("M-<f5>"  . dap-hydra)
              ("M-s d"   . dap-hydra))
  :hook ((after-init     . dap-auto-configure-mode)
         (dap-stopped    . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))

         (python-mode . (lambda () (require 'dap-python)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (go-mode . (lambda () (require 'dap-go)))
         ((js-mode js2-mode) . (lambda () (require 'dap-firefox)))))


;; ----------------------------------------------------------------
;; Language servers
;; ----------------------------------------------------------------

;; C/C++/Objective-C support
(use-package lsp-clangd
  :ensure nil
  :after lsp-mode
  :init
  (when (executable-find "clangd")
    ;; set clangd-args to accelerate clangd parsing
    (defconst clangd-args '("-j=2"
                            "--background-index"
                            "--clang-tidy"
                            "--recovery-ast"
                            "--cross-file-rename"
                            "--completion-style=bundled"
                            "--pch-storage=memory"
                            "--suggest-missing-includes"
                            "--header-insertion=iwyu"
                            "--header-insertion-decorators"))
    (setq lsp-clients-clangd-args clangd-args)))

;; Python: use "pyls" by default in lsp-mode
;; install: "pip install python-language-server"
(use-package lsp-pyls
  :ensure nil
  :if (string-equal *py-langserver* "pyls")
  :after lsp-mode
  :hook (python-mode . lsp)
  :config
  (setq lsp-clients-python-library-directories
        '("/usr/local/lib/" "/usr/lib/")))

;; Python: use Microsoft Python Language Server for Auto-completion
;; use "M-x lsp-python-ms-update-server" to upgrade from Miscrosoft
(use-package lsp-python-ms
  :if (string-equal *py-langserver* "mspyls")
  :after lsp-mode
  :init
  ;; auto download released executable mspyls from Miscrosoft
  (setq lsp-python-ms-auto-install-server t
        lsp-python-ms-cache "Library")        ;; cache parsing
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

;; Python: use Microsoft Pyright language server
;; install by "npm install -g pyright"
(use-package lsp-pyright
  :if (string-equal *py-langserver* "pyright")
  :after lsp-mode
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))


(provide 'init-lsp)
;; ================================================
;; init-lsp.el ends here
