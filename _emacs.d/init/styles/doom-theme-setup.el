;; ================================================================
;; This .el is to configure the doom-theme.
;; ================================================================

;; To use doom-theme, you need to install fonts:
;; - SF Mono, SF Pro Text;
;; - Sarasa Mono SC;
;; - Iosevka;


(use-package doom-themes
  :load-path "~/.emacs.d/themes/github/emacs-doom-themes/"
  :config
  ;; global settings
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; terminal color supports for /doom-one/
  ;; iTerm "background" set to #282C34, and "black" set to #1B2229

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
    (set-default 'neo-window-fixed-size nil)  ;; allow adjust window size

    ;; improves org-mode fontification.
    (doom-themes-org-config)
    ;; customize font pitch
    (require 'org-variable-pitch)
    (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode))
  )



(provide 'doom-theme-setup)
;; ================
;; doom-theme-setup.el ends here
