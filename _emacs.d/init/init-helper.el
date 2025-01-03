;; ===============================================================
;; Keybinding cheatsheet or accelerator
;; ===============================================================
;; Last modified on 01 Jan 2025

;; /which-key/ for keybinding help
(use-package which-key
  :demand
  :bind ("C-h M-m" . which-key-show-major-mode)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :hook (after-init . which-key-mode)

  :config
  ;; use posframe for which-key
  (use-package which-key-posframe
    :demand
    :after which-key
    :custom-face
    (which-key-posframe ((t (:inherit tooltip))))
    (which-key-posframe-border ((t (:inherit posframe-border))))
    :config
    (setq which-key-posframe-border-width 1
          which-key-posframe-poshandler   #'posframe-poshandler-frame-center
          which-key-posframe-parameters   '((left-fringe . 8) (right-fringe . 8)
                                            (min-width   . 75)))
    (which-key-posframe-mode 1)))

;; /hydra/ for quicker keybinding
(use-package hydra
  :demand
  :after posframe
  :init
  (setq hydra-hint-display-type 'posframe)
  ;; config hydra posframe window
  (defun hydra-set-posframe-show-params (origin-fun &rest args)
    "Set hydra-posframe style."
    (setq hydra-posframe-show-params
          `(:left-fringe 14
                         :right-fringe 14
                         :internal-border-width 2
                         :internal-border-color ,(face-background 'posframe-border nil t)
                         :background-color ,(face-background 'tooltip nil t)
                         :foreground-color ,(face-foreground 'tooltip nil t)
                         :lines-truncate t
                         :poshandler posframe-poshandler-frame-center-near-bottom
                         )))
  (advice-add 'hydra-posframe-show :before #'hydra-set-posframe-show-params))

;; /pretty-hydra/ for use-package integration
(use-package pretty-hydra
  :demand
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face 'mode-line-emphasis))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))


(provide 'init-helper)
;; ================================================
;; init-helper.el ends here
