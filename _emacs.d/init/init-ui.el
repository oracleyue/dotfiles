;; ================================================================
;; Emacs Themes
;; ================================================================

;; Banner logo
(defcustom zyue-logo
  (expand-file-name "themes/logo.png" user-emacs-directory)
  "Set banner logo in the splash screen. nil means official logo."
  :type 'string)

;; Frame   (note: [96,36] in Mac; 33 in Thinkpad)
(if *is-mac*
    (setq default-frame-alist '((width . 81) (height . 50)))
  (setq default-frame-alist '((width . 96) (height . 32))))

;; Transparent titlebar for Mac OS X
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; Paths: load-path, theme-load-path
(add-to-list 'load-path
             (expand-file-name "init/misc" user-emacs-directory))
(dolist (subdir '("."
                  "atom-one-dark-theme"))
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
(when *is-mac*   (setq height-n 150 height-s 140))
(when *is-linux* (setq height-n 110 height-s 105))
(setq size-n (/ height-n 10.0))
(defun zyue-search-and-load-fonts (&optional frame)
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
  ;; Specify font for Chinese
  (catch 'loop
    (dolist (font '("Source Han Serif SC" "Source Han Serif TC" "Source Han Serif"  ; 思源宋体 (简中、繁中、日文)
                    "WenQuanYi Micro Hei" "Sarasa Mono SC"
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
        (set-fontset-font t 'unicode font)  ; 4-5th arguments omitted: nil 'prepend
        (throw 'loop t))))
  ;; fixing specific glyghs, if needed
  ;; (set-fontset-font t '(#x26A0 . #x274C) "Apple Color Emoji")

  ;; Rescale fonts; force equal widths (2 EN = 1 CHS)
  ;; (Warning: if LC_CTYPE=zh_CN.UTF-8 in "locale", this will not work)
  ;; (setq face-font-rescale-alist
  ;;       '(("WenQuanYi Micro Hei" . 1.2) ("Sarasa Mono SC" . 1.2)
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
  ;; avoid slowing down performance
  :config (setq inhibit-compacting-font-caches t))

;; Modeline (powerline, spaceline, doomline, plain)
(require 'init-modeline)

;; Themes (eclipse, doom-nord-light; doom-one, atom-one-dark, spacemacs-dark)
(setq zyue-theme 'eclipse)
(when *is-server-c* (setq zyue-theme 'doom-one))
(when *is-terminal* (setq zyue-theme 'spacemacs-dark
                          zyue-modeline 'plain))

(pcase zyue-theme
  ((or 'doom-one 'doom-nord-light)
   (setq zyue-modeline 'doomline)
   (use-package doom-themes
     :custom-face
     (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
     :custom
     (doom-themes-treemacs-theme "doom-colors")
     :config
     ;; Enable flashing mode-line on errors
     (doom-themes-visual-bell-config)
     ;; Enable customized theme
     ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
     (with-eval-after-load 'lsp-treemacs
       (doom-themes-treemacs-config))))
  ((or 'spacemacs-dark 'spacemacs-light)
   (setq zyue-modeline 'spaceline)
   (use-package spacemacs-theme))
  ('eclipse
   (setq zyue-modeline 'powerline)))

;; Dashboard (alternative startup/splash screen)
(when (and *enable-all-the-icons* *is-graphic*)
  (require 'init-dashboard))

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
