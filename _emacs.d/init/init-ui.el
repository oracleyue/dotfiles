;; ================================================================
;; Emacs Themes
;; ================================================================

;; Install required emacs packages
(setq custom/theme-packages
      '(spacemacs-theme))
(custom/install-packages custom/theme-packages)

;; Git clone themes from github:
;; If using "doom-themes", go to "~/.emacs.d/themes" and run "./themes-dl.sh"


;; Frame   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 32))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Loading paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/github")
(add-to-list 'load-path "~/.emacs.d/init/styles")

;; Variables
(defvar zyue-theme nil
  "A symbol representing the color theme to load.")
(defvar zyue-modeline nil
  "A symbol representing the modeline style to load.")
(defvar zyue-font nil
  "The default font to use. Expects a `font-spec'.")
(defvar zyue-modeline-font nil
  "The font to use for modeline. Expects a `font-spec'.")
(defvar zyue-variable-pitch-font nil
  "The default font to use for variable-pitch text. Expects a `font-spec'.")
(defvar zyue-unicode-font nil
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.
Expects a `font-spec'.")

;; Init or reload functions
(defun zyue-init-ui (&optional frame)
  ;; load theme
  (when zyue-theme
    (load-theme zyue-theme t))
  ;; load modeline style
  (zyue-modeline-setup zyue-modeline)
  ;; loading after frame creations
  (when window-system
    ;; transparent titlebar appearance
    (if (eq (frame-parameter frame 'background-mode) 'light)
        (add-to-list 'default-frame-alist '(ns-appearance . light))
      (add-to-list 'default-frame-alist '(ns-appearance . dark)))
    ;; load fonts
    (when (fontp zyue-font)
      (set-frame-font zyue-font nil (if frame (list frame) t))
      (set-face-attribute 'fixed-pitch frame :font zyue-font))
    ;; ... and for Unicode characters
    (when (fontp zyue-unicode-font)
      (set-fontset-font t 'unicode zyue-unicode-font frame))
    ;; ... and for variable-pitch-mode:
    (when (fontp zyue-variable-pitch-font)
      (set-face-attribute 'variable-pitch frame
                          :font zyue-variable-pitch-font))
    ;; ... and for mode-line fonts
    (when (fontp zyue-modeline-font)
      (set-face-attribute 'mode-line nil
                          :font zyue-modeline-font)
      (set-face-attribute 'mode-line-inactive nil
                          :font zyue-modeline-font))))

(defun zyue-reload-ui-in-daemon (frame)
  "Reload the theme (and font) in an daemon frame."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'zyue-init-ui))))

;; Fonts
(if *is-mac*
    (setq
     zyue-font (font-spec :family "SF Mono" :size 15.0)
     zyue-modeline-font (font-spec :family "SF Mono" :size 14.0)
     zyue-unicode-font (font-spec :family "Sarasa Mono SC" :size 14.0)
     ;; ovp-font "Iosevka"  ;; used in /org-variable-pitch.el/
     ovp-font zyue-font
     zyue-variable-pitch-font (font-spec :family "SF Compact Display" :size 15))
  (setq
   zyue-font (font-spec :family "DejaVu Sans Mono" :size 11.0)
   zyue-modeline-font (font-spec :family "DejaVu Sans Mono" :size 10.5)
   zyue-unicode-font (font-spec :family "WenQuanYi Micro Hei" :size 10.5)
   zyue-variable-pitch-font (font-spec :family "Roboto")))

;; Themes for different app and daemons
(setq zyue-theme 'doom-nord-light)
(when (or *is-server-coding* *is-server-linux*)
  (setq zyue-theme 'doom-one))
(when *is-terminal* (setq zyue-theme 'spacemacs-dark))

;; Modeline
(require 'init-modeline)
(if (or *is-server-main* *is-linux*)
    (setq zyue-modeline 'doomline)
  (setq zyue-modeline 'spaceline))  ;; custom

;; Setup themes
(pcase zyue-theme
  ((or 'doom-one 'doom-nord-light) (require 'doom-theme-setup))
  ((or 'spacemacs-dark 'spacemacs-light) (use-package spacemacs-theme :defer t)))

;; UI loading
(if (daemonp)
    (add-hook 'after-make-frame-functions #'zyue-reload-ui-in-daemon)
  (zyue-init-ui))

;; Post-processing (if specific themes require)
(when (functionp 'theme-post-processing)
  (theme-post-processing))
(require 'zyue-ui-neotree)

;; Transparent background (alpha < 1)
(defun new-alpha-frame (&optional value)
  (interactive)
  (or value (setq value 95))
  (make-frame '((alpha . value))))



(provide 'init-ui)
;; ================================================
;; init-ui.el ends here
