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
      :init
      ;; (setq powerline-display-buffer-size nil)
      ;; (setq powerline-display-mule-info nil)
      ;; (setq powerline-display-hud nil)
      :config
      (when (display-graphic-p)
        (powerline-default-theme)
        ;; (remove-hook 'focus-out-hook 'powerline-unset-selected-window)
        ))))

;; ---------------------------------------------
;; Spaceline
;; ---------------------------------------------
(defun zyue-use-spaceline ()
  (use-package spaceline
    :demand
    :init (setq powerline-default-separator 'slant)
    :config
    (spaceline-emacs-theme) ;; (spaceline-spacemacs-theme)
    ;; fix applet bug on OSX
    ;; (setq ns-use-srgb-colorspace nil)  ;; this fix dims colors
    ;; (setq powerline-image-apple-rgb t) ;; better fix, but no longer working
    ))

;; ---------------------------------------------
;; Doomline
;; ---------------------------------------------
(defun zyue-use-doomline ()
  (use-package doom-modeline
    :demand
    :init
    (setq doom-modeline-height 32
          doom-modeline-buffer-file-name-style 'auto)
    (doom-modeline-mode 1)))

;; ---------------------------------------------
;; Wraper for loading modeline
;; ---------------------------------------------
(defun zyue/modeline-setup (&optional theme)
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
