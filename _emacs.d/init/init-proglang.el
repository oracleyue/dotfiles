;; ================================================================
;; Programming Supports for Minority Languages
;; ================================================================


;; ---------------------------------------------
;; major mode for /VimScript/ (e.g. ".vimrc")
;; ---------------------------------------------
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))


;; ---------------------------------------------
;; major mode for /AppleScript/
;; ---------------------------------------------
(autoload 'applescript-mode "applescript-mode"
  "Major mode for editing AppleScript source." t)
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))



(provide 'init-proglang)
;; ================================================
;; init-proglang.el ends here
