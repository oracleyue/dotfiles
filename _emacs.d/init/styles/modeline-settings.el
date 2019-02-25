;; ================================================================
;; Mode-Line Customizations
;; ================================================================


;; ---------------------------------------------
;; Spaceline
;; ---------------------------------------------
(defun zyue-use-spaceline ()
  (use-package spaceline
    :ensure t
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
  (use-package shrink-path :ensure t)
  (use-package eldoc-eval :ensure t)
  (use-package all-the-icons :ensure t)
  (use-package doom-modeline
    :ensure t
    :config
    (setq doom-modeline-height 30)
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


(provide 'modeline-settings)
;; ================================================
;; modeline-settings.el ends here
