;; ===============================================================
;; Hydra configurations
;; ===============================================================
;; Last modified on 23 Dec 2024

;; /hydra/ for quick keybinding
(use-package hydra
  :demand
  :after posframe
  :init
  (setq hydra-hint-display-type 'posframe)
  ;; config hydra posframe window
  (defun hydra-set-posframe-show-params ()
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
  ;; loading for daemon starting
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame
                             (hydra-set-posframe-show-params))))
    (hydra-set-posframe-show-params)))

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


(provide 'init-hydra)
;; ================================================
;; init-hydra.el ends here
