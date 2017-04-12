;; ---------- For Windows 7 Usage -------------
;; Remember to add your home directory HOME in windows envi vaiables.
;; Remember to unzip the color-theme lib to your .emacs.d folder.
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))
 ;; Setting size of frames
(when window-system (set-frame-size (selected-frame) 80 40))
 ;; oracleyue's key bindings
(global-set-key (kbd "C-c C-c") 'comment-region)
    ; For conflicts with Windows 7 hot keys
    (define-key global-map "\C-x m" 'set-mark-command)
 ;; Default path for openning files
(setq default-directory "~/Desktop/")

 ;; Setting color-theme lib
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
	 (color-theme-subtle-hacker)))

 ;; Adding path for plugins
 (add-to-list 'load-path "~/.emacs.d/")
 (load "package.el")
 ;; For Package Management by ELPA
 (require 'package)
 ;; Any add to list for package-archives (to add marmalade or melpa) goes here
     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
     (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
 (package-initialize)

 ;;; ---------- Settings of Modes -------------
 ;
 ;; Enable /linum/ in nlinum lib
 (require 'linum)
     (global-linum-mode 1)
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ;; Enable /evil-mode/
 ;(require 'evil)
 ;    (evil-mode 1)
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ;; Settings for /Org-mode/
 (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
     ;; global keys for Org mode
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)
     (setq org-startup-indented t)
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ;; For /MATLAB-Mode/
 ;; Replace path below to be where your matlab.el file is.
 (add-to-list 'load-path "~/.emacs.d/matlab-emacs/")
 (load-library "matlab-load")
 ;; Enable CEDET feature support for MATLAB code. (Optional)
 ; (matlab-cedet-setup)
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
