;; ================================================================
;; Emacs Themes
;; ================================================================

;; Download themes from github:
;; If using "doom-themes" or "eclipse-themes", go to "~/.emacs.d/themes"
;; and run "./themes-dl.sh"


;; Banner logo
(defcustom zyue-logo
  (expand-file-name "themes/logo.png" user-emacs-directory)
  "Set banner logo in the splash screen. nil means official logo."
  :type 'string)

;; Frame   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 96) (height . 36)))
  (setq default-frame-alist '((width . 96) (height . 32))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Paths: load-path, theme-load-path
(add-to-list 'load-path
             (expand-file-name "init/styles" user-emacs-directory))
(dolist (subdir '("."
                  "atom-one-dark-theme"
                  "github/eclipse-theme"
                  "github/emacs-doom-themes"))
  (let ((theme-dir (expand-file-name (concat "themes/" subdir)
                                     user-emacs-directory)))
    (when (file-directory-p theme-dir)
      (add-to-list 'custom-theme-load-path theme-dir))))

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
    (check-and-load-fonts frame)
    ;; refresh dashboard
    (when (get-buffer "*dashboard*")
      (dashboard-refresh-buffer))
    ;; fix faces
    (fix-faces frame)))

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
(when *is-mac*   (setq height-n 150 height-s 140))
(when *is-linux* (setq height-n 110 height-s 105))
(setq size-n (/ height-n 10.0))
(defun check-and-load-fonts (&optional frame)
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
  ;; Specify font for unicode symbols
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t))))
  ;; Specify font for Chinese
  (catch 'loop
    (dolist (font '("WenQuanYi Micro Hei" "Sarasa Mono SC"
                    "PingFang SC" "Microsoft Yahei"))
      (when (member font (font-family-list))
        ;; Note: when LC_CTYPE=zh_CN.UTF-8, use (find-font (font-spec :name font))
        ;; since Chinese font names appear in (font-family-list) as unicode codes.
        (dolist (charset '(kana han cjk-misc bopomofo))  ;; remove "symbol"
          (set-fontset-font (frame-parameter nil 'font)
  		                    charset
  		                    (font-spec :family font :size size-n)))
        (throw 'loop t))))
  ;; Rescale fonts; force equal widths (2 EN = 1 CHS)
  ;; (Warning: if LC_CTYPE=zh_CN.UTF-8 in "locale", this will not work)
  (setq face-font-rescale-alist
        '(("WenQuanYi Micro Hei" . 1.2) ("Sarasa Mono SC" . 1.2)
          ("PingFang SC" . 1.2)    ("Microsoft Yahei" . 1.2))))

;; Fix faces that fail to display correctly in some themes, OS or monitors
(defun fix-faces (&optional fram)
  ;; Fix face bugs in ivy-switch-buffer
  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-org nil :font zyue-font :weight 'bold))
  ;; Multiple-cursors (the other cursor bars are invisible in Linux)
  (when (and *is-linux* (eq zyue-theme 'doom-nord-light))
    (with-eval-after-load 'multiple-cursors
      (set-face-attribute 'mc/cursor-bar-face nil :background "#5272AF")))
  )

;; Modeline (powerline, spaceline, doomline, plain)
(require 'init-modeline)
(setq zyue-modeline 'doomline)

;; Themes (eclipse, doom-nord-light, doom-one, atom-one-dark)
(setq zyue-theme 'doom-nord-light)
(when *is-server-coding* (setq zyue-theme 'doom-one))
(when *is-terminal*
  (setq zyue-theme 'doom-one zyue-modeline 'plain))

;; Setup themes
(pcase zyue-theme
  ((or 'doom-one 'doom-nord-light) (require 'doom-theme-setup))
  ((or 'spacemacs-dark 'spacemacs-light)
   (use-package spacemacs-theme))
  ('eclipse (use-package eclipse-theme
              :load-path "themes/github/eclipse-theme"
              :demand)))

;; Dashboard (alternative startup/splash screen)
(require 'init-dashboard)

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
