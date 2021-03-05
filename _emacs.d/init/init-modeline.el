;; ================================================================
;; Modeline Customizations
;; ================================================================
;; Last modified on 20 Feb 2020

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
        (remove-hook 'focus-out-hook 'powerline-unset-selected-window))))
  )

;; ---------------------------------------------
;; Spaceline
;; ---------------------------------------------
(defun zyue-use-spaceline ()
  (use-package spaceline
    :demand
    :init (setq powerline-default-separator 'slant)
    :config
    (spaceline-spacemacs-theme)  ;(spaceline-emacs-theme)
    ;; fix applet bug on OSX
    (when *is-mac*
      (setq powerline-image-apple-rgb t)))
  )

;; ---------------------------------------------
;; Doomline
;; ---------------------------------------------
(defun zyue-use-doomline ()
  (use-package doom-modeline
    :demand
    :init
    (setq doom-modeline-height 25
          ;; use buffer name; show the full-path name when mouse hovering
          doom-modeline-buffer-file-name-style 'truncate-upto-root)
    :config (doom-modeline-init))
  )

;; ---------------------------------------------
;; Wraper for loading modeline
;; ---------------------------------------------
(defun zyue-modeline-setup (&optional theme)
  "Interface to load the theme for modeline."
  (pcase theme
    ('plain       (require 'plain-modeline))
    ('powerline   (zyue-use-powerline))
    ('spaceline   (zyue-use-spaceline))
    ('doomline    (zyue-use-doomline))
    (_            nil)))


(provide 'init-modeline)
;; ================================================
;; init-modeline.el ends here
