;; ================================================================
;; Use Eglot as a light LSP client
;; ================================================================
;; Last modified on 08 Mar 2021


(use-package eglot
  :demand
  :config
  (dolist (hook (list
                 ;; 'js-mode-hook
                 ;; 'rust-mode-hook
                 'python-mode-hook
                 ;; 'ruby-mode-hook
                 ;; 'java-mode-hook
                 ;; 'sh-mode-hook
                 ;; 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 ;; 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (eglot-ensure))))

  ;; C/C++/Objective-C support
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
  ;; set clangd-args to accelerate clangd parsing
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" clangd-args)))

  ;; Python: use pyls (default); pyright problemic
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . ("pyright-langserver" "--stdio")))

  ) ;END of eglot


(provide 'init-eglot)
;; ================================================
;; init-eglot.el ends here
