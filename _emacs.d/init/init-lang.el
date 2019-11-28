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
  :demand
  :load-path "~/.emacs.d/git"
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

;; ---------------------------------------------
;; major mode for /AppleScript/
;; ---------------------------------------------
(use-package apples-mode
  :demand
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;; ---------------------------------------------
;; major mode for /YAML/ (*.yml)
;; ---------------------------------------------
(use-package yaml-mode
  :demand
  :load-path "~/.emacs.d/git"
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(provide 'init-lang)
;; ================================================
;; init-lang.el ends here
