;; ================================================================
;; Emacs Themes
;; ================================================================

;; Banner logo
(defcustom zyue-logo
  (expand-file-name "themes/logo.png" user-emacs-directory)
  "Set banner logo in the splash screen. nil means official logo."
  :type 'string)

;; Frame   (note: [96, 33] in Thinkpad)
(setq default-frame-alist '((width . 92) (height . 56))) ;; 85/52

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

;; Fonts
(if *is-mac* (setq size-n 15.0) (setq size-n 10.5))
(defun zyue/search-and-load-fonts (&optional frame)
  ;; Specify default/fixed-width fonts
  (catch 'loop
    (dolist (font '("JetBrainsMono Nerd Font"
                    "FiraCode Nerd Font"
                    "RobotoMono Nerd Font" ;; fix: disable "medium" ttf!
                    "SF Mono"              ;; Mac only
                    "Consolas"))           ;; Windows only
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
    (dolist (font '("Times New Roman" "Roboto"))
      (when (member font (font-family-list))
        (set-face-attribute 'variable-pitch frame :font font)
        (throw 'loop t))))
  ;; Specify font for Chinese
  (catch 'loop
    (dolist (font '("LXGW WenKai Mono"         ;; 霞鹜文楷
                    "WenQuanYi Micro Hei Mono"
                    "Source Han Serif SC"      ;; 思源宋体 (简、繁、日)
                    "PingFang SC" "Microsoft Yahei"))
      (when (member font (font-family-list))
        ;; Note: when LC_CTYPE=zh_CN.UTF-8, use (find-font (font-spec :name font))
        ;; since Chinese font names appear in (font-family-list) as unicode codes.
        (dolist (charset '(kana han cjk-misc bopomofo))  ;; symbol
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family font)))
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
  (setq face-font-rescale-alist
        '(("LXGW WenKai Mono" . 1.2)
          ("WenQuanYi Micro Hei Mono" . 1.0)))
  ) ;; Font Loading

;; Icons supports
(if (eq *icons-type* 'nerd-icons)
    ;; use /nerd-icons/
    (use-package nerd-icons
      :demand
      :custom (nerd-icons-font-family "Symbols Nerd Font Mono")
      :config
      ;; set ".m" as matlab filetype
      (setf (alist-get "m" nerd-icons-extension-icon-alist nil nil #'equal)
            '(nerd-icons-devicon "nf-dev-matlab")))
  ;; use /all-the-icons/
  (use-package all-the-icons
    :init (unless (or (find-font (font-spec :name "all-the-icons"))
                      (daemonp))
            (all-the-icons-install-fonts t))
    :config
    (setq all-the-icons-scale-factor 1.0)  ;; adjust size
    ;; avoid slowing down performance
    (setq inhibit-compacting-font-caches t)))

(defun icons-displayable-p (&optional type)
  "Return non-nil if icons are displayable."
  (let ((nerd-icons-p (or (featurep 'nerd-icons)
                          (require 'nerd-icons nil t)))
        (all-the-icons-p (or (featurep 'all-the-icons)
                             (require 'all-the-icons nil t))))
    (if type
        (and (eq *icons-type* type)
             (or (if (eq *icons-type* 'nerd-icons)
                     nerd-icons-p
                   nil)
                 (if (eq *icons-type* 'all-the-icons)
                     all-the-icons-p
                   nil)))
      (or nerd-icons-p all-the-icons-p))))

;; Themes
;; (eclipse, doom-nord-light; doom-one, spacemacs-dark, tao-yang, elegant-light)
(setq zyue-theme 'doom-one)
(when *is-server-m* (setq zyue-theme 'elegant-light))
(when *is-server-c* (setq zyue-theme 'doom-one))
(when *is-terminal* (setq zyue-theme 'spacemacs-dark))

(pcase zyue-theme
  ((or 'doom-one 'doom-nord-light)
   (setq zyue-modeline 'doomline)
   (use-package doom-themes
     :custom
     (doom-themes-treemacs-theme "doom-colors")
     (line-spacing nil) ;; 0.1 for FiraCode
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
  (_ nil))

;; Transparent effect (alpha < 1)
(defun zyue/set-frame-alpha (active inactive)
  "Set frame alpha values for active and inactive states.
ACTIVE: opacity when frame is active (0-100)
INACTIVE: opacity when frame is inactive (0-100)"
  (interactive "nActive frame opacity (0-100): \nnInactive frame opacity (0-100): ")
  (let ((values (cons active inactive)))
    (add-to-list 'default-frame-alist `(alpha . ,values))
    ;; Apply to existing frames
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha values))))

;; Init or reload functions
(defun zyue/init-ui (&optional frame)
  ;; load theme
  (when zyue-theme
    (load-theme zyue-theme t))
  ;; load modeline style
  (zyue/modeline-setup zyue-modeline)
  ;; loading after frame creations
  (when window-system
    ;; update transparent titlebar textcolor wrt themes
    (modify-frame-parameters
     frame `((ns-appearance . ,(frame-parameter frame 'background-mode))))
    ;; transparent background
    (zyue/set-frame-alpha 100 95)
    ;; load fonts
    (zyue/search-and-load-fonts frame)))

(defun zyue/reload-ui-in-daemon (frame)
  "Reload the theme (and font) in an daemon frame."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'zyue/init-ui))))

;; UI loading
(if (daemonp)
    (add-hook 'after-make-frame-functions #'zyue/reload-ui-in-daemon)
  (zyue/init-ui))

;; /Posframe/ for floating windows
(use-package posframe
  :demand
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border `((t (:background "gray50")))
    "Face used by the `posframe' border."
    :group 'posframe)
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))


(provide 'init-ui)
;; ================================================
;; init-ui.el ends here
