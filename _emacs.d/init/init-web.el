;; ================================================================
;; Programming Environment for Web Development
;; ================================================================


;; Use /html-mode/ for html

;; Use /css-mode/ for css

;; Use /nxml-mode/ for xml
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; Use /js2-mode/ for javescript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

;; /emmet-mode/ for zen-coding
;; useage: "C-j" to expand
(use-package emmet-mode
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; Use /json-mode/ for json
(use-package json-mode
  :config
  ;; /popwin/: manage popup (temporary) buffers
  ;; BUG: it disables /neotree/ to create buffers.
  (use-package popwin
    :disabled
    :config
    (popwin-mode 1)

    (defun popwin:json-mode-show-path ()
      "Add json-path in json-mode to a popup window."
      (interactive)
      (json-mode-show-path)
      (delete-windows-on "*json-path*")
      (popwin:popup-buffer "*json-path*"))
    (push "*json-path*" popwin:special-display-config)  ;; not work, fix later
    (add-hook 'json-mode-hook
              (lambda () (define-key json-mode-map
                      (kbd "C-c C-p") 'popwin:json-mode-show-path)))
    ) ;; END of use-package popwin
  ) ;; END of use-package json-mode

;; Usages:
;; - "C-c C-f"   format the region/buffer with =json-reformat=
;; - "C-c C-p"   display a path to the object at point with =json-snatcher=
;; - "C-c P"     copy a path to the object at point to the kill ring with =json-snatcher=
;; - "C-c C-t"   Toggle between =true= and =false= at point
;; - "C-c C-k"   Replace the sexp at point with null
;; - "C-c C-i"   Increment the number at point
;; - "C-c C-d"   Decrement the number at point

;; ----------------------------------------------------------------
;; Use comprehensive mode /web-mode/ for web development
;;
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.[x]?html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
;; ----------------------------------------------------------------


(provide 'init-web)
;; ================================================
;; init-web.el ends here
