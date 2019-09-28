;; ================================================================
;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
;; ================================================================

;; Install LSP language servers
;; - Python: pip install python-language-server

;; Warning: you have to keep "dash" and "company" modes update-to-date
;; whenever update lsp packages.


(use-package lsp-mode
  ;; :demand
  :hook ((python-mode . lsp))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; Configure LSP clients
  (use-package lsp-clients
    :ensure nil
    :init
    (setq lsp-clients-python-library-directories
          '("/usr/local/lib/" "/usr/lib/"))))

(use-package lsp-ui
  ;; :demand
  ;; :custom-face (lsp-ui-doc-background ((t (:background nil))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'top
              lsp-ui-doc-border (face-foreground 'default)

              lsp-ui-sideline-enable nil
              lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  ;; :demand
  :init (setq company-lsp-cache-candidates 'auto))


(provide 'init-lsp)
;; ================================================
;; init-lsp.el ends here
