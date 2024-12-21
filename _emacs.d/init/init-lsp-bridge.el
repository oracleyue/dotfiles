;; ================================================================
;; The Fastest Language Server Protocol
;; ================================================================
;; Last modified on 11 Aug 2022

;; To-do's:
;; - disable lsp-bridge to complete yas snippets for matlab-mode

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
  :diminish " (LSP/b)"
  :hook ((prog-mode . lsp-bridge-mode))
  :bind (:map prog-mode-map
              ("C-." . lsp-bridge-find-def)
              ("C-," . lsp-bridge-find-def-return)
              ("M-s D" . lsp-bridge-find-def-other-window)
              ("M-s i" . lsp-bridge-find-impl)
              ("M-s I" . lsp-bridge-find-impl-other-window)
              ("M-s r" . lsp-bridge-find-references)
              ("M-s h" . lsp-bridge-show-documentation)
              ("M-s c" . lsp-bridge-signature-help-fetch)  ;; function call tips
              ("M-s ;" . lsp-bridge-rename)
              ("M-s a" . lsp-bridge-code-action)
              ("M-s F" . lsp-bridge-code-format)
              ("M-s p" . lsp-bridge-peek)  ;; in peek, further "M-l j/b" jump/back
              ;; use "M-S-n/-p" to select item; use "M-n/-p" scroll peek contents
              ("M-s s" . lsp-bridge-workspace-list-symbols)
              ("M-s l" . lsp-bridge-diagnostic-list))
  :config
  (yas-global-mode 1)
  ;; (global-lsp-bridge-mode)    ;; mode-specific enabled instead

  ;; completion interface
  (setq acm-enable-yas t
        acm-backend-yas-candidate-min-length 3)

  ;; ---------------- Python ----------------
  ;; Python virtualenv support
  (setq lsp-bridge-python-command "/usr/local/bin/python") ;; which has /epc/ installed
  (defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
    (let* ((json-object-type 'plist)
           (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
           (custom-config (expand-file-name "pyright.json" custom-dir))
           (default-config (json-read-file (expand-file-name "site-lisp/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
           (settings (plist-get default-config :settings)))

      (plist-put settings :pythonPath (executable-find "python"))
      (make-directory (file-name-directory custom-config) t)
      (with-temp-file custom-config
        (insert (json-encode default-config)))
      custom-config))
  (add-hook 'python-mode-hook
            (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))
  (add-hook 'pyvenv-post-activate-hooks
            (lambda () (lsp-bridge-restart-process)))

  ;; ---------------- MATLAB ----------------
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(octave-mode . "matlab-ls"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'octave-mode-hook)
  (add-to-list 'lsp-bridge-formatting-indent-alist '(octave-mode . octave-block-offset))

  ) ;; End of lsp-bridge


;; Avoid conflicting, if company-mode enabled
;; (when *use-company*
;;   (setcdr (last company-global-modes) '(python-mode emacs-lisp-mode)))

;; Disable lsp-bridge for certain major modes (if load for "prog-mode")
;; (add-hook 'octave-mode-hook (lambda () (lsp-bridge-mode -1)))


(provide 'init-lsp-bridge)
;; ================================================
;; init-lsp-bridge.el ends here
