;; ================================================================
;; Emacs Themes
;; ================================================================


;; Frame Size   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 33))))

;; Theme Path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")
(add-to-list 'load-path "~/.emacs.d/themes/sanityinc-tomorrow")

;; Customize ModeLine
(add-to-list 'load-path "~/.emacs.d/init/styles")
(load "customize-modeline" t t)

;; Setup Theme
(cond (*is-mac*
       (cond
        ;; app (not server-client)
        ((not (daemonp))
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; GUI
           (load-theme 'Amelie t)))               ;; terminal
        ;; servers (use daemon)
        (*is-server-main*                         ;; server: main
         (load-theme 'solarized t))
        (*is-server-coding*
         (add-hook 'after-make-frame-functions    ;; server: coding
                   (lambda (frame)
                     (select-frame frame)
                     (when (display-graphic-p frame)
                       (load-theme 'atom-one-dark t)
                       (y:setup-mode-line)))))
        (*is-server-ac*
         (add-hook 'after-make-frame-functions    ;; server: ac-mode
                   (lambda (frame)
                     (select-frame frame)
                     (when (display-graphic-p frame)
                       (load-theme 'monokai t)
                       (y:setup-mode-line)))))))
      (*is-linux*
       (if (or (daemonp) (display-graphic-p))
           (load-theme 'atom-one-dark t)          ;; GUI (app or server)
         (load-theme 'Amelie t))))                ;; terminal
(y:setup-mode-line)

;; ;; Set Fringe Color
;; (when (eq 'atom-one-dark (car custom-enabled-themes))
;;   (set-face-attribute 'fringe nil
;;                       :foreground (face-foreground 'default)
;;                       :background (face-background 'mode-line)))


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
