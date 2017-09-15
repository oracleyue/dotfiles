;; use /html-mode/ for html
;; use /css-mode/ for css

;; use /js2-mode/ for javescript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; setup /emmet-mode/ for Zen Coding
;; useage: "C-j" to expand
(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode) ;; auto-start on any web files
(add-hook 'css-mode-hook 'emmet-mode) ;; auto-start on any web files


;; setup /web-mode/ as major-modes for web development
;(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.[x]?html\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))



(provide 'emacs-init-web)
;; ================================================
;; emacs-init-web.el ends here