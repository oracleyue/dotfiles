;; ================================================================
;; General Programming Supports
;; ================================================================

;; ---------------------------------------------
;; /magit/: version control
;; ---------------------------------------------
(global-set-key (kbd "C-c h g") 'magit-status)


;; ----------------------------------------------
;; /linum/: adding line number on left fringe
;; ----------------------------------------------
;; basic linum padding
;; (setq linum-format "%4d  ")

;; customize and stylize linum
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)

;; enable linum-mode
(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'matlab-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'c-mode-common-hook 'linum-mode)
(add-hook 'ess-mode-hook 'linum-mode)   ; for R
(add-hook 'makefile-gmake-mode-hook 'linum-mode) ; default for gnome
(add-hook 'makefile-bsdmake-mode-hook 'linum-mode) ; default for OS X


;; ----------------------------------------------
;; /iedit/: edit the same variable everywhere (keystroke "C-c ;")
;; ----------------------------------------------
(require 'iedit)
(when (eq 'monokai (car custom-enabled-themes))
  (set-face-attribute 'iedit-occurrence nil
                      :foreground "#272822"
                      :background "#E6DB74"
                      :weight 'normal))


;; ----------------------------------------------
;; API reference support
;; ----------------------------------------------
;; Integration with /Dash/ for quick refernce (only available for Mac OS X)
(when (string-equal system-type "darwin")
  (require 'dash-at-point)
  (global-set-key (kbd "C-c d") 'dash-at-point) ;; "C-c d", conflicts with /doxyemacs/
  ; specify docsets to search in different modes
  (set 'dash-at-point-mode-alist
       '((c-mode . "c,gsl,gl4")
         (c++-mode . "cpp,eigen,boost,gsl")
         (python-mode . "python,numpy,scipy,matplotlib,pandas")
         (ess-mode . "r")
         (sh-mode . "bash"))))
;; Integration with /Zeal/ for quick refernce (available for Linux)
(when (string-equal system-type "gnu/linux")
  (require 'zeal-at-point)
  (global-set-key (kbd "C-c d") 'zeal-at-point)
  (set 'dash-at-point-mode-alist
       '((c-mode . "c,gsl,gl4")
         (c++-mode . "c++,eigen,boost,gsl")
         (python-mode . "python 2,numpy,scipy,matplotlib,pandas")
         (ess-mode . "r"))))


;; ----------------------------------------------
;; /gud/: debug supports, e.g. gdb, pdb
;; ----------------------------------------------
;; - usages: "M-x pdb", "M-x gud-gdb"
;;           "M-x gdb" then "M-x gdb-many-window", then "b main" "r"
;; - notes: keybindings compatible with JetBrains
(when (string-equal system-type "darwin")
  (setq gdb-non-stop-setting nil))
(global-set-key (kbd "<f7>") 'gud-step);; equiv step in
(global-set-key (kbd "<f8>") 'gud-next) ;; equiv step over
(global-set-key (kbd "<f9>") 'gud-cont) ;; equiv continue
(global-set-key (kbd "S-<f8>") 'gud-finish) ;; equiv step out


;; ----------------------------------------------
;; /ECB/: GUI interface of IDE
;; ----------------------------------------------
;; [SOURCE]: https://github.com/emacsmirror/ecb

(when nil
  (add-to-list 'load-path "~/.emacs.d/git/ecb")
  (require 'ecb)
  (require 'ecb-autoloads)
  (setq ecb-windows-width 0.16)
  (setq ede-project-directories (quote ("~/Workspace/c")))
  (setq ecb-source-path '("~/Workspace/c/src"
                          "~/Workspace/c/include"))
  (setq ecb-tip-of-the-day nil)
  (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

  (add-hook 'c-mode-hook 'ecb-activate)
  (add-hook 'c++-mode-hook 'ecb-activate)
  (add-hook 'matlab-mode-hook 'ecb-activate))



(provide 'init-progsupp)
;; ================================================
;; init-progsupp.el ends here
