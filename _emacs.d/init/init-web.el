;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; install required packages for web development
(setq custom/web-packages
      '(js2-mode
        emmet-mode
        json-reformat
        json-snatcher
        json-mode))
(custom/install-packages custom/web-packages)

;; use /html-mode/ for html
;; use /css-mode/ for css

;; use /js2-mode/ for javescript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; /emmet-mode/ for zen-coding
;; useage: "C-j" to expand
(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode) ;; auto-start on any web files
(add-hook 'css-mode-hook 'emmet-mode) ;; auto-start on any web files

;; use /json-mode/ for json
(require 'json-mode)
;; add json-path to popup window
(require 'popwin)
(defun popwin:json-mode-show-path ()
  (interactive)
  (json-mode-show-path)
  (delete-windows-on "*json-path*")
  (popwin:popup-buffer "*json-path*"))
(push "*json-path*" popwin:special-display-config)  ;; not work, fix later
(add-hook 'json-mode-hook
          (lambda () (define-key json-mode-map
                       (kbd "C-c C-p") 'popwin:json-mode-show-path)))
;; usages:
;; - "C-c C-f"   format the region/buffer with =json-reformat=
;; - "C-c C-p"   display a path to the object at point with =json-snatcher=
;; - "C-c P"     copy a path to the object at point to the kill ring with =json-snatcher=
;; - "C-c C-t"   Toggle between =true= and =false= at point
;; - "C-c C-k"   Replace the sexp at point with null
;; - "C-c C-i"   Increment the number at point
;; - "C-c C-d"   Decrement the number at point


;; ----------------------------------------------------------------
;;
;; use comprehensive mode /web-mode/ for web development
;;
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.[x]?html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
;; ----------------------------------------------------------------



(provide 'init-web)
;; ================================================
;; init-web.el ends here
