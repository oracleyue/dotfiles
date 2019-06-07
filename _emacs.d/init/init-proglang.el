;; ================================================================
;; Programming Supports for Minority Languages
;; ================================================================

;; Install required Emacs packages
(setq custom/proglang-packages
      '(apples-mode
        julia-mode))
(custom/install-packages custom/proglang-packages)


;; ---------------------------------------------
;; major mode for /VimScript/ (e.g. ".vimrc")
;; ---------------------------------------------
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))


;; ---------------------------------------------
;; major mode for /AppleScript/
;; ---------------------------------------------
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")


;; ---------------------------------------------
;; major mode for /YAML/ (*.yml)
;; ---------------------------------------------
(use-package yaml-mode
  :load-path "~/.emacs.d/git"
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))



(provide 'init-proglang)
;; ================================================
;; init-proglang.el ends here
