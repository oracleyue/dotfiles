;; ================================================================
;; /Helm/ as the main completion system
;; ================================================================

;; Install required packages for more functions
(setq custom/helm-packages
      '(helm
        helm-core))
(custom/install-packages custom/helm-packages)

;; configuration for /helm/
(require 'helm)
(require 'helm-config)

;; Basic keybinding:
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; More basic settings:
(setq ;; open helm buffer inside current window, not occupy whole other window
      helm-split-window-in-side-p           t
      ;; move to end or beginning of source when reaching top or bottom of source.
      helm-move-to-line-cycle-in-source     t
      ;; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp        t
      ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-scroll-amount                    8
      ; helm-candidate-number-limit           500
      ; helm-move-to-line-cycle-in-source     t
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(setq ;; default way to split window
      ; helm-split-window-default-side    'right
      ;; adjust the max height for helm buffer
      ; helm-autoresize-max-height 50
      ;; auto-resize buffer to fit candidates (disble golden-ratio)
      helm-autoresize-mode              t)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x b") 'helm-mini)
;; enable fuzzy matching
(setq helm-buffers-fuzzy-matching   t
      helm-recentf-fuzzy-match      t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; skip meaningless files, e.g. .DS_Store
(setq helm-ff-skip-boring-files t)
(delete '"\\.bbl$" helm-boring-file-regexp-list)    ;show .bbl file
(add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
(add-to-list 'helm-boring-file-regexp-list ".*\.synctex\.gz$")
(add-to-list 'helm-boring-file-regexp-list ".*\.url$")
(add-to-list 'helm-boring-file-regexp-list "\\.dropbox$")
(add-to-list 'helm-boring-file-regexp-list "Icon.*")
(add-to-list 'helm-boring-file-regexp-list "#.*#$")
(add-to-list 'helm-boring-file-regexp-list "\\.out$")

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; use helm to show the mark-ring
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "M-g SPC") 'helm-all-mark-rings)

;; use helm to show the register
(global-set-key (kbd "C-c h x") 'helm-register)

;; use helm to show /eshell/ command history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook #'(lambda ()
   (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; set global-key for function: =M-g a= in session of =C-x C-f=
(global-set-key (kbd "M-g a") 'helm-do-grep-ag)

(setq helm-grep-default-command     ;; ~ack~
      "ack -Hn --color --smart-case --no-group %e %p %f"
      helm-grep-default-recurse-command
      "ack -H --color --smart-case --no-group %e %p %f")

(setq helm-ls-git-grep-command    ;; ~git-grep~
      "git grep -n%cH --color=always --full-name -e %p %f")

(setq helm-grep-ag-command        ;; ~ag~ from "the-silver-searcher"
      "ag --line-numbers -S --hidden --color --color-match '31;43' \
          --nogroup %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(with-eval-after-load 'helm-semantic      ;; default: C, python, elisp
  (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (push '(emacs-lisp-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (nbutlast helm-semantic-display-style 2)) ;; remove the default elisp setting

;; helm for Emacs help functions
(setq helm-apropos-fuzzy-match t)

(setq helm-lisp-fuzzy-completion t)

;; helm for system man page
(when (string-equal system-type "darwin")
  (setq helm-man-format-switches "%s"))
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; helm find
(global-set-key (kbd "M-g /") 'helm-find)

;; helm for system command "locate"
(setq helm-locate-fuzzy-match t)

;; ===============================================================
;; Settings for /helm-swoop/
;; ===============================================================

;; Install required packages for more functions
(setq custom/helm-swoop-packages
      '(helm-swoop))
(custom/install-packages custom/helm-swoop-packages)

(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-g o") 'helm-swoop)    ; default "M-i"
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "M-g O") 'helm-swoop-back-to-last-point) ;default "M-I"
(global-set-key (kbd "C-c M-o") 'helm-multi-swoop) ;default "C-c M-i"
(global-set-key (kbd "C-x M-o") 'helm-multi-swoop-all) ;default "C-x M-i"

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m")
  'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; ===============================================================
;; Project Management via Projectile
;; ===============================================================

;; Install required packages for more functions
(setq custom/proj-packages
      '(projectile
        helm-projectile))
(custom/install-packages custom/proj-packages)

;; /projectile/: project management
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; /helm-projectile/: browse via helm
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

(setq projectile-switch-project-action 'helm-projectile)

;; adding the switch between html <-> js
;; html -> js
(add-to-list 'projectile-other-file-alist '("html" "js"))
;; js -> html
(add-to-list 'projectile-other-file-alist '("js" "html"))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "run")
     (add-to-list 'grep-find-ignored-directories "obj")))

;; Ignore files/directories
(add-to-list 'projectile-globally-ignored-files "*.out")
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
(add-to-list 'projectile-globally-ignored-directories ".git")
;; retore projectile's native indexing (fix the bug of disabling .projectile)
(setq projectile-indexing-method 'native)

;; Safe Variable Declaration (suppress warnings)
;; (add-to-list 'safe-local-variable-values
;;              '(project-local-include-path . ("-I./include" "-I./src")))

;; ================================================================
;; Tag Supports in Programming Environement
;; ================================================================

;; Install required packages for more functions
(setq custom/gtags-packages
      '(helm-gtags))
(custom/install-packages custom/gtags-packages)

(setenv "GTAGSLABEL" "pygments")
(setenv "GTAGSLIBPATH" (concat (getenv "HOME") "/.gtags/")) ;; if tag system libs

(require 'helm-gtags)
(setq helm-gtags-ignore-case             t
      helm-gtags-auto-update             t
      helm-gtags-use-input-at-cursor     t
      helm-gtags-pulse-at-cursor         t
      helm-gtags-prefix-key              "\C-cg"
      helm-gtags-suggested-key-mapping   t)

;; Enable helm-gtags mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'makefile-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'matlab-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)    ;(definitions)
(define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)   ;(references)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol) ;(symbols)

(define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-parse-file)
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)

(define-key helm-gtags-mode-map (kbd "C-c g [") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c g ]") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)

(define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)
(define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags)

(provide 'init-helm)
;; ================================================
;; init-helm.el ends here
