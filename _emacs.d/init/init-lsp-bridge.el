;; ================================================================
;; The Fastest Language Server Protocol
;; ================================================================
;; Last modified on 11 Aug 2022

;; System dependencies:
;; - Python package: "pip install epc"
;; - LSP for Python: "pip install pyright"
;; Elisp dependencies:
;; - postframe
;; - markdown-mode
;; - yasnippet

(use-package lsp-bridge
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :diminish " (LSP/g)"
  :hook ((prog-mode . lsp-bridge-mode))
  :bind (:map prog-mode-map
              ("C-." . lsp-bridge-find-def)
              ("C-," . lsp-bridge-return-from-def)
              ("M-s ." . lsp-bridge-find-def)
              ("M-s ," . lsp-bridge-return-from-def)
              ("M-s D" . lsp-bridge-find-def-other-window)
              ("M-s i" . lsp-bridge-find-impl)
              ("M-s I" . lsp-bridge-find-impl-other-window)
              ("M-s r" . lsp-bridge-find-references)
              ("M-s h" . lsp-bridge-lookup-documentation)
              ("M-s ;" . lsp-bridge-rename)
              ("M-s l" . lsp-bridge-list-diagnostics))
  :config
  (yas-global-mode 1)
  ;; (global-lsp-bridge-mode)    ;; mode-specific enabled instead

  ;; acm completion
  ;; (setq acm-backend-lsp-candidates-max-number 4000) ;; for large pkg like cv2

  ;; LSP for Python
  ;; pyright stubPath: set "stubPath" in "lsp-bridge/langserver/pyright.json"
  ;;    "stubPath": "/Users/zyue/Programs/python/python-type-stubs",
  )

;; Avoid conflicting, if company-mode enabled
(when *use-company*
  (setcdr (last company-global-modes) '(python-mode emacs-lisp-mode)))

;; Disable lsp-bridge for certain major modes
(add-hook 'octave-mode-hook (lambda () (lsp-bridge-mode -1)))


(provide 'init-lsp-bridge)
;; ================================================
;; init-lsp-bridge.el ends here
