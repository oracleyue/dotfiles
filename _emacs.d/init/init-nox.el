;; ================================================================
;; Use manateelazycat's Nox as a light LSP client
;; ================================================================
;; Last modified on 08 Mar 2021


(use-package nox
  :ensure nil
  :load-path "site-lisp/nox/"
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
    (add-hook hook '(lambda () (nox-ensure))))

  ;; C/C++/Objective-C support
  (add-to-list 'nox-server-programs
               '((c++-mode c-mode) . ("clangd")))

  ;; Python: use pyls, mspyls, pyright
  (setq nox-python-path "/usr/local/bin/python")
  (setq nox-python-server "mspyls")
  ;; set path for mspyls
  (when (string-equal nox-python-server "mspyls")
    (setq nox-python-server-dir
          (expand-file-name ".cache/lsp/mspyls/" user-emacs-directory)))

  ) ;END of nox


(provide 'init-nox)
;; ================================================
;; init-nox.el ends here
