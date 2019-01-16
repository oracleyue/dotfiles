(use-package lsp-mode
  :load-path "git/lsp-mode"
  :config
  (setq lsp-project-blacklist '("^/usr/")
        lsp-highlight-symbol-at-point nil)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(use-package lsp-ui
  :load-path "git/lsp-ui"
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-max-width 50
        )
  :bind (:map lsp-ui-peek-mode-map
              ("h" . lsp-ui-peek--select-prev-file)
              ("j" . lsp-ui-peek--select-next)
              ("k" . lsp-ui-peek--select-prev)
              ("l" . lsp-ui-peek--select-next-file)
              :map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references)
              ))

(use-package company-lsp
  :load-path "git/company-lsp"
  :after (company lsp-mode)
  :init
  (setq company-lsp-cache-candidates nil)
  (add-hook 'lsp-mode-hook
            (lambda()
              (add-to-list (make-local-variable 'company-backends)
                           'company-lsp)))
  )

(use-package flycheck
  :ensure t
  :defer t)

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package python
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --no-color-info"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(use-package lsp-python
  :load-path "git/lsp-python"
  :after python
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))

(provide 'init-lsp)
;; ================================================
;; init-lsp.el ends here
