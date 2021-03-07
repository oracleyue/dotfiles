;; ================================================================
;; Programming Supports for Minority Languages
;; ================================================================
;; Last modified on 07 Mar 2021

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
