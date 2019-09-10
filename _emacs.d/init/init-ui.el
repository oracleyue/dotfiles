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
    ;; transparent background
    (when *is-linux* (set-bg-alpha '(100 85)))
    ;; check and choose fonts
    (check-and-load-fonts frame)))

(defun zyue-reload-ui-in-daemon (frame)
  "Reload the theme (and font) in an daemon frame."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'zyue-init-ui))))

;; Transparent effect (alpha < 1)
(defun set-bg-alpha (value)
  "This function set the Alpha value of frames to make background
transparent. VALUE is a list (A, AB), where A is the Alpha value
of the focused frame and AB is the unfocused."
  (set-frame-parameter (selected-frame) 'alpha value)
  (add-to-list 'default-frame-alist (cons 'alpha value)))

;; Fonts
(defun check-and-load-fonts (&optional frame)
  (when *is-mac*   (setq size-n 15 height-s 140))
  (when *is-linux* (setq size-n 11 height-s 105))
  ;; Specify default/fixed-width fonts
  (catch 'loop
    (dolist (font '("SF Mono" "DejaVu Sans Mono" "RobotoMono"
                    "Inconsolata" "Menlo" "Consolas"))
      (when (member font (font-family-list))
        (setq zyue-font (font-spec :family font :size size-n)
              ovp-font zyue-font) ;; used in /org-variable-pitch.el/
        (set-face-attribute 'default frame :font zyue-font)
        (set-face-attribute 'fixed-pitch frame :font zyue-font)
        (set-face-attribute 'mode-line frame :height height-s)
        (set-face-attribute 'mode-line-inactive frame :height height-s)
        (throw 'loop t))))
  ;; Specify variable-width font
  (catch 'loop
    (dolist (font '("SF Compact Display" "DejaVu Sans" "Roboto"))
      (when (member font (font-family-list))
        (set-face-attribute 'variable-pitch frame :font font)
        (throw 'loop t))))
  ;; Specify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t))))
  ;; Specify font for Chinese
  (catch 'loop
    (dolist (font '("Sarasa Mono SC" "WenQuanYi Micro Hei"
                    "PingFang SC" "Microsoft Yahei"))
      (when (member font (font-family-list))
        (dolist (charset '(kana han cjk-misc bopomofo)) ;; symbol
          (set-fontset-font (frame-parameter nil 'font)
  		                    charset
  		                    (font-spec :family font :size size-n)))
        ;; rescale to equal widths (2 EN = 1 SC)
        (setq face-font-rescale-alist
              '(("Sarasa Mono SC" . 1.2) ("WenQuanYi Micro Hei" . 1.2)
                ("PingFang SC"    . 1.2) ("Microsoft Yahei"     . 1.2)))
        (throw 'loop t))))
  )

;; Themes for different app and daemons
(setq zyue-theme 'doom-nord-light)
(when *is-server-coding* (setq zyue-theme 'doom-one))  ;; doom-one
(when *is-terminal* (setq zyue-theme 'spacemacs-dark))

;; Modeline
(require 'init-modeline)
(setq zyue-modeline 'doomline) ;; spaceline

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
  (theme-post-processing))  ;; for doom-themes

;; Loop over transparent effects
;; (global-set-key [(f11)] 'loop-alpha)
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab) (set-bg-alpha (list a ab)))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))



(provide 'init-ui)
;; ================================================
;; init-ui.el ends here
