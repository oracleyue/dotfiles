;; ================================================================
;; This .el is to configure the doom-theme.
;; ================================================================

;; (add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-themes/")
;; (require 'doom-themes)

(use-package doom-themes
  :defer t
  :config
  ;; global settings
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; user font settings
  (setq
   ;; ovp-font "Iosevka"  ;; used in /org-variable-pitch.el/
   zyue-font (font-spec :family "SF Mono" :size 15)
   zyue-modeline-font (font-spec :family "SF Mono" :size 14)
   zyue-variable-pitch-font
   (font-spec
    :family "SF Compact Display"
    :size 14
    :width 'extra-condensed
    :weight 'normal
    :slant 'normal
    :registry "iso10646-1")
   zyue-unicode-font (font-spec :family "Sarasa Mono SC" :size 14))

  ;; default theme
  (when (equal zyue-theme 'doom-theme)
    (setq zyue-theme 'doom-one))

  ;; post-processing
  (defun theme-post-processing ()
    ;; enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; enable custom neotree theme
    (setq doom-neotree-enable-variable-pitch t
          doom-neotree-project-size 1.2
          doom-neotree-line-spacing 0.5
          doom-neotree-folder-size 1.0
          doom-neotree-chevron-size 0.6)
    (doom-themes-neotree-config)  ; /all-the-icons/ required!

    ;; improves org-mode fontification.
    (doom-themes-org-config)
    ;; customize font pitch
    (require 'org-variable-pitch)
    (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)))



(provide 'doom-theme-settings)
;; ================
;; doom-theme-settings.el ends here