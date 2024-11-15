;; ================================================================
;; Emacs Themes
;; ================================================================

;; Banner logo
(defcustom zyue-logo
  (expand-file-name "themes/logo.png" user-emacs-directory)
  "Set banner logo in the splash screen. nil means official logo."
  :type 'string)

;; Frame   (note: [96, 33] in Thinkpad)
(setq default-frame-alist '((width . 90) (height . 48)))

;; Transparent titlebar for Mac OS X
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; Paths: load-path, theme-load-path
(add-to-list 'load-path
             (expand-file-name "init/misc/" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" user-emacs-directory))

;; Variables
(defvar zyue-theme nil
  "A symbol representing the color theme to load.")
(defvar zyue-modeline nil
  "A symbol representing the modeline style to load.")
(defvar zyue-font nil
  "The default font to use. Expects a `font-spec'.")
(defvar font-userdefine-flag t
  "Set nil if your theme specifies a font for `default'.")

;; Modeline (powerline, spaceline, doomline, plain)
(require 'init-modeline)

;; Init or reload functions
(defun zyue-init-ui (&optional frame)
  ;; load theme
  (when zyue-theme
    (load-theme zyue-theme t))
  ;; load modeline style
  (zyue-modeline-setup zyue-modeline)
  ;; loading after frame creations
  (when window-system
    ;; update transparent titlebar textcolor wrt themes
    (modify-frame-parameters frame `((ns-appearance . ,(frame-parameter frame 'background-mode))))
    ;; transparent background
    (when *is-linux* (set-bg-alpha '(100 85)))
    ;; choose and load fonts
    (zyue-search-and-load-fonts frame)
    ;; refresh dashboard
    (when (and (get-buffer "*dashboard*")
               (not (buffer-file-name))) ;; hide dashboard when edit files
      (dashboard-refresh-buffer))))

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
(if *is-mac* (setq size-n 14.0) (setq size-n 10.5))
(defun zyue-search-and-load-fonts (&optional frame)
  ;; Specify default/fixed-width fonts
  (catch 'loop
    (dolist (font '("Roboto Mono"
                    ;; fix bug: remove/disable "medium"-type ttf files!
                    "DejaVu Sans Mono" ;; Linux default
                    "SF Mono"  ;; Mac only
                    "Consolas" ;; Windows default
                    "Sarasa Mono SC Nerd"))  ;; width compatible with Chinese
      (when (member font (font-family-list))
        (setq zyue-font (font-spec :family font :size size-n))
        (when font-userdefine-flag
          (set-face-attribute 'default frame :font zyue-font)
          (set-face-attribute 'fixed-pitch frame :font zyue-font))
        ;; (set-face-attribute 'mode-line frame :height (* size-n 10))
        ;; (set-face-attribute 'mode-line-inactive frame :height (* size-n 10))
        (throw 'loop t))))
  ;; Specify variable-width font
  (catch 'loop
    (dolist (font '("Times New Roman"
                    "Roboto"
                    "SF Compact Display"  ;; Mac only
                    "DejaVu Sans"))
      (when (member font (font-family-list))
        (set-face-attribute 'variable-pitch frame :font font)
        (throw 'loop t))))
  ;; Specify font for Chinese
  (catch 'loop
    (dolist (font '("Source Han Serif SC" "Source Han Serif"  ;; 思源宋体 (简中、繁中、日文)
                    "WenQuanYi Micro Hei"
                    "Sarasa Mono SC Nerd"  ;; width compatible with Chinese
                    "PingFang SC" "Microsoft Yahei"))
      (when (member font (font-family-list))
        ;; Note: when LC_CTYPE=zh_CN.UTF-8, use (find-font (font-spec :name font))
        ;; since Chinese font names appear in (font-family-list) as unicode codes.
        (dolist (charset '(kana han cjk-misc bopomofo))  ;; symbol
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family font )))
        (throw 'loop t))))
  ;; Specify font for unicode symbols
  (catch 'loop
    (dolist (font '("Apple Color Emoji" "Apple Symbols" "Symbola"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font) ; 4-5th arguments omitted to force use Emoji: nil 'prepend
        (throw 'loop t))))
  ;; fixing specific glyghs, if needed
  ;; (set-fontset-font t '(#x26A0 . #x274C) "Apple Color Emoji")

  ;; Rescale fonts; force equal widths (2 EN = 1 CHS)
  ;; (Warning: if LC_CTYPE=zh_CN.UTF-8 in "locale", this will not work)
  ;; (setq face-font-rescale-alist
  ;;       '(("WenQuanYi Micro Hei" . 1.2)
  ;;         ("Sarasa Mono SC" . 1.2)
  ;;         ("Source Han Serif SC" . 1.2) ("Source Han Serif" . 1.2)
  ;;         ("PingFang SC" . 1.2)    ("Microsoft Yahei" . 1.2)))
  )

;; Icons
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and *enable-all-the-icons*
       (require 'all-the-icons nil t)))

(use-package all-the-icons
  :if *enable-all-the-icons*
  :init (unless (or (font-installed-p "all-the-icons")
                    (daemonp))
          (all-the-icons-install-fonts t))
  :config
  (setq all-the-icons-scale-factor 1.0)  ;; adjust size
  ;; avoid slowing down performance
  (setq inhibit-compacting-font-caches t))

;; Themes (eclipse, doom-nord-light; doom-one, spacemacs-dark, tao-yang, elegant-light)
(setq zyue-theme 'doom-one)
(when *is-server-m* (setq zyue-theme 'elegant-light))
(when *is-server-c* (setq zyue-theme 'doom-one))
;; (when *is-server-c* (setq zyue-theme 'spacemacs-dark))
(when *is-terminal* (setq zyue-theme 'spacemacs-dark
                          zyue-modeline 'plain))

(pcase zyue-theme
  ((or 'doom-one 'doom-nord-light)
   (setq zyue-modeline 'doomline)
   (use-package doom-themes
     :custom (doom-themes-treemacs-theme "doom-colors")
     :config (doom-themes-visual-bell-config)))
  ((or 'spacemacs-dark 'spacemacs-light)
   (setq zyue-modeline 'powerline)
   ;; (setq zyue-modeline 'spaceline)  ;; bugs on redisplay
   (use-package spacemacs-theme))
  ('eclipse  ;; clone from abo-abo's eclipse-theme
   (setq zyue-modeline 'powerline)
   (use-package eclipse-theme
     :demand
     :load-path "themes/eclipse-theme/"
     :ensure nil
     :init (require 'more-faces-eclipse-theme)))
  ((or 'tao-yang 'tao-ying)
   (setq zyue-modeline 'doomline)
   (use-package tao-theme
     :demand
     :load-path "themes/tao-theme-emacs/"
     :ensure nil
     :config
     (add-to-list 'default-frame-alist '(internal-border-width . 24))))
  ((or 'elegant-light 'elegant-dark)
   (use-package elegant-theme
     :ensure nil
     :demand
     :load-path "themes/elegant-theme/"
     :init   (setq elegant-modeline-disabled nil)
     :config (setq font-userdefine-flag nil)))
  )

;; Dashboard (alternative startup/splash screen)
(require 'init-dashboard)

;; UI loading
(if (daemonp)
    (add-hook 'after-make-frame-functions #'zyue-reload-ui-in-daemon)
  (zyue-init-ui))

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
