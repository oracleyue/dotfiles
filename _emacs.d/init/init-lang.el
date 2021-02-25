;; ================================================================
;; Programming Supports for Minority Languages
;; ================================================================


;; ---------------------------------------------
;; major mode for distraction-free writing
;; ---------------------------------------------
(use-package olivetti
  :demand
  :hook ((olivetti-mode . (lambda () (auto-fill-mode -1)))
         (olivetti-mode . hide-mode-line-mode))
  :init
  (setq olivetti-body-width 0.65)
  :config
  ;; hide modeline
  (use-package hide-mode-line)
  ;; aliasing
  (defalias 'writing-mode 'olivetti-mode))

;; ---------------------------------------------
;; major mode for /VimScript/ (e.g. ".vimrc")
;; ---------------------------------------------
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

;; ---------------------------------------------
;; major mode for /AppleScript/
;; ---------------------------------------------
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;; ---------------------------------------------
;; major mode for /YAML/ (*.yml)
;; ---------------------------------------------
(use-package yaml-mode
  :mode "\\.yml\\'")

;; ---------------------------------------------
;; major mode for /TOML/ (*.toml)
;; ---------------------------------------------
(use-package toml-mode
  :mode "\\.toml\\'")


(provide 'init-lang)
;; ================================================
;; init-lang.el ends here
