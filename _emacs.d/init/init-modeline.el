;; ================================================================
;; Modeline Customizations
;; ================================================================


;; ---------------------------------------------
;; Powerline
;; ---------------------------------------------
(defun zyue-use-powerline ()
  (when (image-type-available-p 'xpm)
    (use-package powerline
      :demand
      :config
      (setq powerline-display-buffer-size nil)
      (setq powerline-display-mule-info nil)
      (setq powerline-display-hud nil)
      (when *is-mac*  ;; fix applet bug on OSX
        (setq powerline-image-apple-rgb t))
      (when (display-graphic-p)
        (powerline-default-theme)
        (remove-hook 'focus-out-hook 'powerline-unset-selected-window)))))

;; ---------------------------------------------
;; Spaceline
;; ---------------------------------------------
(defun zyue-use-spaceline ()
  (use-package spaceline
    :ensure nil
    :demand
    :config
    (require 'spaceline-config)
    (when *is-mac*  ;; fix applet bug on OSX
      (setq powerline-image-apple-rgb t))
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
    :demand
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
    ('powerline (zyue-use-powerline))
    ('spaceline (zyue-use-spaceline))
    ('doomline  (zyue-use-doomline))
    (_          (zyue-customize-modeline))
    ))

;; ---------------------------------------------
;; Misc for Modeline (e.g., Nyan cat, parrot)
;; ---------------------------------------------
(use-package nyan-mode
  :ensure nil
  :disabled
  :init (setq nyan-bar-length 24)
  (nyan-mode))


(provide 'init-modeline)
;; ================================================
;; init-modeline.el ends here
