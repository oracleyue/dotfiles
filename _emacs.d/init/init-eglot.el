;; ================================================================
;; Use Eglot as a light LSP client
;; ================================================================
;; Last modified on 08 Mar 2021

(use-package eglot
  :demand
  :config
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'python-mode-hook
                 ;; 'js-mode-hook
                 'haskell-mode-hook
                 ;; 'rust-mode-hook
                 ;; 'ruby-mode-hook
                 ;; 'java-mode-hook
                 ;; 'sh-mode-hook
                 ;; 'php-mode-hook
                 ))
    (add-hook hook '(lambda () (eglot-ensure))))

  ;; set checker
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  ;; eldoc
  (setq eldoc-echo-area-use-multiline-p 1)

  ;; --------------------------------------------
  ;; Language Servers
  ;; --------------------------------------------
  ;; C/C++/Objective-C support
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd")))

  ;; Python: use pyls (default);
  ;; Note: pyright is problemic since it can only see python stdlib.
  (when (string-equal *py-langserver* "pyright")
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio"))))

  ;; Python: use mspyls
  (when (string-equal *py-langserver* "mspyls")
    (defclass eglot-mspyls (eglot-lsp-server) ()
      :documentation
      "MS Python Language Server.")

    (setq-default eglot-workspace-configuration
                  '((:python :autoComplete (:extraPaths nil)
                             :analysis (:autoSearchPaths :json-false :usePYTHONPATH :json-false))))

    (cl-defmethod eglot-initialization-options ((_server eglot-mspyls))
      `(:interpreter
        (:properties
         (:InterpreterPath "/usr/local/bin/python"))))

    (setq mspyls-cmd "~/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer")
    (add-to-list 'eglot-server-programs (list 'python-mode mspyls-cmd)))

  ) ;END of eglot


(provide 'init-eglot)
;; ================================================
;; init-eglot.el ends here
