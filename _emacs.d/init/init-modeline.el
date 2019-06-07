;; ================================================================
;; Modeline Customizations
;; ================================================================

;; Install required emacs packages
(setq custom/modeline-packages
      '(spaceline
        doom-modeline
        shrink-path
        eldoc-eval
        all-the-icons))
(custom/install-packages custom/modeline-packages)


;; ---------------------------------------------
;; Spaceline
;; ---------------------------------------------
(defun zyue-use-spaceline ()
  (use-package spaceline
    :config
    (require 'spaceline-config)
    (when *is-mac*
      (setq powerline-image-apple-rgb t))    ;; fix applet bug on OSX
    (spaceline-emacs-theme)))  ;; OR spaceline-spacemacs-theme

;; ---------------------------------------------
;; Doomline
;; ---------------------------------------------
(defun zyue-use-doomline ()
  ;; dependencies
  (use-package shrink-path)
  (use-package eldoc-eval)
  (use-package all-the-icons)
  (use-package doom-modeline
    :ensure t
    :config
    (setq doom-modeline-height 30)
    ;; use buffer name; show the full-path file name when moving mouse over it
    ;; (setq doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-init)))

;; ---------------------------------------------
;; Customized modeline
;; ---------------------------------------------
(defun zyue-customize-modeline ()
  (require 'zyue-modeline))

;; Wraper function to load modeline
(defun zyue-modeline-setup (&optional theme)
  "Interface to load the theme for modeline."
  (pcase theme
    ('custom    (zyue-customize-modeline))
    ('spaceline (zyue-use-spaceline))
    ('doomline  (zyue-use-doomline))
    (_          (zyue-customize-modeline))
    ))

;; ---------------------------------------------
;; Misc for Modeline (e.g., Nyan cat, parrot)
;; ---------------------------------------------
(use-package nyan-mode
  :disabled
  :init (setq nyan-bar-length 24)
  (nyan-mode))


(provide 'init-modeline)
;; ================================================
;; init-modeline.el ends here
