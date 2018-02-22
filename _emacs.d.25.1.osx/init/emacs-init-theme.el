;; ================================================================
;; Emacs Themes
;; ================================================================


;;
;; Frame Size   (note: [96,36] in Mac; 33 in Thinkpad)
;;
(if (string-equal system-type "darwin")
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 33))))


;;
;; Theme Path
;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")


;;
;; Customize ModeLine
;;
(load-file (expand-file-name "init/styles/customize-modeline.el"
			     user-emacs-directory))


;;
;; Setup Theme
;;
(cond ((string-equal system-type "darwin")
       (cond
        ;; app (not server-client)
        ((not (daemonp))
         (if (display-graphic-p)
             (load-theme 'atom-one-dark t)        ;; GUI
           (load-theme 'Amelie t)))               ;; terminal
        ;; servers (use daemon)
        ((string-equal "main" (daemonp))          ;; server: main
         (load-theme 'solarized t))
        ((string-equal "coding" (daemonp))
         (add-hook 'after-make-frame-functions    ;; server: coding
                   (lambda (frame)
                     (select-frame frame)
                     (when (display-graphic-p frame)
                       (load-theme 'atom-one-dark t)
                       (y:setup-mode-line)))))
        ((string-equal "ac-mode" (daemonp))
         (load-theme 'monokai t))))               ;; server: ac-mode
      ((string-equal system-type "gnu/linux")
       (if (or (daemonp) (display-graphic-p))
           (load-theme 'atom-one-dark t)          ;; GUI (app or server)
         (load-theme 'Amelie t))))               ;; terminal
(y:setup-mode-line)


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



(provide 'emacs-init-theme)
;; ================================================
;; emacs-init-theme.el ends here
