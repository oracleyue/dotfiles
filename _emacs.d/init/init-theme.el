;; ================================================================
;; Emacs Themes
;; ================================================================

;; Install required emacs packages
(setq custom/theme-packages
      '(;powerline
        spaceline
        spacemacs-theme))
(custom/install-packages custom/theme-packages)


;; Frame Size   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 33))))


;; Font Size (Mac/Linux)
(defun y:adjust-default-fontsize ()
  (if *is-mac*      ;; default: 13/15(mac), 10.5/12(linux)
      (set-face-attribute 'default nil
                          :font "DejaVu Sans Mono-15")
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12"))
  (if *is-mac*      ;; adjust modeline font
      (progn
        (set-face-attribute 'mode-line nil
                            :font "DejaVu Sans Mono-14")
        (set-face-attribute 'mode-line-inactive nil
                            :font "DejaVu Sans Mono-14"))
    (set-face-attribute 'mode-line nil
                        :font "DejaVu Sans Mono-10.5")
    (set-face-attribute 'mode-line-inactive nil
                        :font "DejaVu Sans Mono-10.5"))
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
                 (y:adjust-default-fontsize)))))
(cond (*is-mac*
       (cond
        ;; apps
        ((not (daemonp))
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; app
           (load-theme 'spacemacs-dark t))        ;; terminal
         (y:adjust-default-fontsize))
        ;; servers (use daemon)
        (*is-server-main*                         ;; server: main
         (server-load-theme 'solarized))
        (*is-server-coding*                       ;; server: coding
         (server-load-theme 'atom-one-dark))
        (*is-server-ac*                           ;; server: ac-mode
         (server-load-theme 'monokai)))
       (y:setup-modeline "spaceline"))
      (*is-linux*
       (if (daemonp)
           (server-load-theme 'atom-one-dark)     ;; server
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; app
           (load-theme 'spacemacs-dark t)))       ;; terminal
       (y:setup-modeline "spaceline")
       (y:adjust-default-fontsize)))


;; Frame with Transparent Background (alpha < 1)
(defun new-alpha-frame (&optional value)
  (interactive)
  (or value (setq value 95))
  (make-frame '((alpha . value))))



(provide 'init-theme)
;; ================================================
;; init-theme.el ends here
