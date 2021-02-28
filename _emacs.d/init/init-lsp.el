;; ================================================================
;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
;; ================================================================
;; Last modified on 24 Feb 2020

;; Install LSP language servers
;; - Python: pip install python-language-server
;;
;; Warning: you have to keep "dash" and "company" modes update-to-date
;; whenever update lsp packages.

;; Usages:
;; - use "projectile" to start a workspace or use "lsp-workspace-folders-add".
;; - use "lsp-describe-sessions" to check status


(use-package lsp-mode
  :demand
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t          ; detect project root
        lsp-prefer-flymake nil         ; use lsp-ui and flycheck
        lsp-completion-provider :none) ; diable add company-capf, set by myself
  :config
  ;; accelearate string concat in elisp
  (setq read-process-output-max (* 1024 1024))

  ;; enable log only for debug
  (setq lsp-log-io nil)

  ;; call signature help
  (setq lsp-signature-render-documentation nil)  ;; keep tip, hide doc
  (setq lsp-signature-doc-lines 3)

  ;; disable uncessary features for better performance
  (setq lsp-enable-folding             nil
        lsp-enable-snippet             nil
        ;; lsp-enable-symbol-highlighting nil
        lsp-enable-links               nil)
  ;; (push "[/\\\\]\\node_modules$" lsp-file-watch-ignored)
  )

(use-package lsp-ui
  :after lsp-mode
  ;; :custom-face (lsp-ui-doc-background ((t (:background nil))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)
              ("M-RET" . lsp-ui-doc-focus-frame))
  :config (setq
           ;; lsp-ui-doc
           lsp-ui-doc-include-signature t
           lsp-ui-doc-position 'top   ;; at-point
           lsp-ui-doc-max-height 15
           ;; lsp-ui-imenu
           lsp-ui-imenu-auto-refresh t
           ;; lsp-ui-sideline
           lsp-ui-sideline-show-code-actions t
           lsp-ui-sideline-show-hover nil
           lsp-ui-sideline-show-diagnostics nil
           lsp-ui-sideline-ignore-duplicate t)
  :config
  (setq lsp-ui-doc-enable      t
        lsp-ui-imenu-enable    t
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable     nil))


(provide 'init-lsp)
;; ================================================
;; init-lsp.el ends here
