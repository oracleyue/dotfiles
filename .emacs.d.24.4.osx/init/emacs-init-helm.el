;; ====================================
;; configuration for /helm/
(require 'helm)
(require 'helm-config)
;; Warning: semantic-mode causes "M-x gdb" hangs emacs in Mac OS X!
;; (if (string-equal system-type "darwin")
;;     (setq y-enable-semantic-parse "no")
;;   (setq y-enable-semantic-parse "yes"))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; Keybinding: "C-o" to switch between Helm sources (e.g. jump from the historical list to the current list)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      ;helm-candidate-number-limit           500
      ;helm-move-to-line-cycle-in-source     t      
      )

(helm-mode 1)

;; default way to split window 
;(setq helm-split-window-default-side 'right)

;; auto-resize buffer to fit candidates
(helm-autoresize-mode t)
;; if using /golden-ratio/
;(defun pl/helm-alive-p ()
;  (if (boundp 'helm-alive-p)
;      (symbol-value 'helm-alive-p)))
;(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; adjust the max height for helm buffer
;(setq helm-autoresize-max-height 50)

;; use helm to fire M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; use helm to show kill-ring (basically, C-y cycle the kill ring)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; use helm to deal with mini-buffer
(global-set-key (kbd "C-x b") 'helm-mini)
;; enable fuzzy matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; use helm to find/open files
;; -keybinding: C-x c C-x C-f; in this session, "C-c i" will insert the current absolute path into the current buffer.
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; use live grep in helm
;; - usage: invoke =helm-ff-run-grep= by =C-s= to search a file/directory on highlighted entry in the helm buffer, when being in a =helm-find-files= session
;; - usage: with prefix =C-u=, recursively grep a selected directory
;; - usage: in *hgrep* buffer, press =C-h m= to view all key bindings
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; /Programming Environment Supports/
;;
;; enable fuzzy matching for "semantic" and "Imenu" listing
;; - keybinding: C-x c i
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
;; restore UI
(with-eval-after-load 'helm-semantic
  (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  ;(push '(c-mode . semantic-format-tag-name) helm-semantic-display-style)
  ;(push '(c-mode . semantic-format-tag-prototype) helm-semantic-display-style)
  (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style))
;; dependency config
;; enable /Imenu/ rescan for helm-semantic-or-imenu
;(setq imenu-auto-rescan t)
;;; enable /semantic-mode/ in /CEDET/ for helm-semantic-or-imenu
(cond ((string-equal y-enable-semantic-parse "yes")
       (semantic-mode 1)
       ;; setting GNU /global/ for /semantic-symref/
       (setq semantic-symref-tool 'global)
       ))

;; use helm to quick-jump to any man entry
;; - keybinding: C-x c m
;(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; use Unix "find" in helm interface
;; - keybinding: C-x c /

;; use Unix "locate" in helm interface
;; - keybinding: C-x c l
(setq helm-locate-fuzzy-match t)

;; use helm to show "occur"
;; - keybinding: C-x c M-s o
;(global-set-key (kbd "C-c h o") 'helm-occur)

;; use helm to show help doc "C-h a"
;; - keybinding: C-x c a
(setq helm-apropos-fuzzy-match t)

;; use helm to show info
;; - keybinding: C-x c h <key> (<key>: g, i, r)

;; use helm to do completion for Lisp
;; - keybinding: C-x c <tab>
(setq helm-lisp-fuzzy-completion t)

;; resum the previous helm session
;; - keybinding: C-x c b

;; view the local/global mark rings in helm
;; - keybinding: C-x c C-c SPC
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

;; use helm to build regexp, test them interactively
;; - keybinding: C-x c r
;; - functions:
;;       [F1] save the regexp as a string in =kill-ring=
;;       [F2] invoke =query-replace= with curent regexp to be replace
;;       [F3] save the regexp as is in the current helm prompt

;; view Emacs registers
;; - keybinding: C-x c C-x r i
;; - functions:
;;       [F1] insert register content into buffer
;;       [F2] append an active region to current content in highlighting register
;;       [F3] prepend an active region to current content in highlighting register
(global-set-key (kbd "C-c h x") 'helm-register)

;; show Unix "top" in helm interface
;; - keybinding: C-x c t
;; - functions:
;;       [C-c C-u]  refresh "helm-top"
;;       [M-c]      sort by ~shell commands~
;;       [M-p]      sort by ~cpu usage~
;;       [M-u]      sort by ~user~
;;       [M-m]      sort by ~memory~

;; fast Unix comand /surfraw/ line interface to popular WWW search engines
;; - keybinding: C-x c s

;; interactively search Google in helm buffer
;; - keybinding: C-x c C-c g
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; quickly view and copy hexadecimal values of colors
;; - keybinding: C-x c c

;; instant eval Emacs Lisp expression in helm buffer
;; - keybinding: C-x c C-:
;(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

;; use Unix gnu-"calc" command in helm interface
;; - keybinding: C-x c C-,

;; use helm to show /eshell/ command history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; use helm to show /shell/ command history
;(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; --------------------------------------
;; Package: helm-projectile

;; --------------------------------------
;; Package: helm-descbinds

;; --------------------------------------
;; Summary of keybindings
;  M-x 	        helm-M-x 	                        List commands
;  M-y 	        helm-show-kill-ring 	            Shows the content of the kill ring
;  C-x b        helm-mini 	                        Shows open buffers, recently opened files
;  C-x C-f 	    helm-find-files 	                The helm version for find-file
;  C-s 	        helm-ff-run-grep 	                Run grep from within helm-find-files
;  C-o                                              Switch between Helm sources in helm session
;  C-c h i 	    helm-semantic-or-imenu 	            Helm interface to semantic/imenu
;  C-c h m 	    helm-man-woman 	                    Jump to any man entry
;  C-c h / 	    helm-find 	                        Helm interface to find
;  C-c h l 	    helm-locate 	                    Helm interface to locate
;  C-c h o 	    helm-occur 	                        Similar to occur
;  C-c h a 	    helm-apropos 	                    Describes commands, functions, variables, …
;  C-c h h g 	helm-info-gnus 	 
;  C-c h h i 	helm-info-at-point 	 
;  C-c h h r 	helm-info-emacs 	 
;  C-c h <tab> 	helm-lisp-completion-at-point 	    Provides a list of available functions
;  C-c h b 	    helm-resume 	                    Resumes a previous helm session
;  C-h SPC 	    helm-all-mark-rings 	            Views content of local and global mark rings
;  C-c h r 	    helm-regex 	                        Visualizes regex matches
;  C-c h x 	    helm-register 	                    Shows content of registers
;  C-c h t 	    helm-top 	                        Helm interface to top
;  C-c h s 	    helm-surfraw 	                    Command line interface to many web search engines
;  C-c h g 	    helm-google-suggest 	            Interactively enter search terms and get results from Google in helm buffer
;  C-c h c 	    helm-color 	                        Lists all available faces
;  C-c h M-: 	helm-eval-expression-with-eldoc 	Get instant results for emacs lisp expressions in the helm buffer
;  C-c h C-, 	helm-calcul-expression 	            Helm interface to calc
;  C-c C-l 	    helm-eshell-history 	            Interface to eshell history
;  C-c C-l 	    helm-comint-input-ring 	            Interface to shell history
;  C-c C-l 	    helm-mini-buffer-history 	        Interface to mini-buffer history
