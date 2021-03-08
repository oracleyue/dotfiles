;; ================================================================
;; Use manateelazycat's Nox as a light LSP client
;; ================================================================
;; Last modified on 08 Mar 2021


(use-package nox
  :ensure nil
  :load-path "site-lisp/nox/"
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
    (add-hook hook '(lambda () (nox-ensure)))))
