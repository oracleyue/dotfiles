;; ================================================================
;; Emacs Themes
;; ================================================================


;; Frame Size   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 33))))

;; Font Size (Mac/Linux)
(defun y:adjust-fontsize ()
  (if *is-mac*      ;; default: 13/15(mac), 10.5/12(linux)
      (set-face-attribute 'default nil
                          :font "DejaVu Sans Mono-15")
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12"))
  (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "Roboto"))

;; Theme Path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")

;; Customize ModeLine
(add-to-list 'load-path "~/.emacs.d/init/styles")
(load "customize-modeline" t t)

;; Setup Theme
(defun server-load-theme (theme)
  (add-hook 'after-make-frame-functions
            `(lambda (frame)
               (select-frame frame)
               (when (display-graphic-p frame)
                 (load-theme ',theme t)
                 (y:setup-modeline)
                 (y:adjust-fontsize)))))
(cond (*is-mac*
       (cond
        ;; apps
        ((not (daemonp))
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; app
           (load-theme 'Amelie t)))               ;; terminal
        ;; servers (use daemon)
        (*is-server-main*                         ;; server: main
         (server-load-theme 'solarized))
        (*is-server-coding*                       ;; server: coding
         (server-load-theme 'atom-one-dark))
        (*is-server-ac*                           ;; server: ac-mode
         (server-load-theme 'monokai))))
      (*is-linux*
       (if (daemonp)
           (server-load-theme 'atom-one-dark)     ;; server
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; app
           (load-theme 'Amelie t)))))             ;; terminal
(unless (daemonp)
  (y:setup-modeline) (y:adjust-fontsize))

;; Frame with Transparent Background (alpha < 1)
(defun new-alpha-frame (&optional value)
  (interactive)
  (or value (setq value 95))
  (make-frame '((alpha . value))))


;; -----------------------------------------------------------------
;; INFO: (available themes and usages)
;; -----------------------------------------------------------------
;; - using /solarized/ theme: Ethan Schoonover's theme [github]
;; (setq solarized-termcolors 256)      ;; used in terminal
;; (setq frame-background-mode 'dark)   ;; use dark theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
;; (load-theme 'solarized t)
;;
;; - using /monokai/ theme; modified by oracleyue
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'monokai t)
;; -----------------------------------------------------------------



(provide 'init-theme)
;; ================================================
;; init-theme.el ends here
