;; ======================================================
;; Tag Supports in Programming Environement
;; ======================================================


;;
;; ---------- Source Code Navigation via TAGS -----------
;;

;; Tagging system: /GNU global/ with ctags+pygments supports
;; installation:
;;   - OS X: "brew install global --with-ctags --with-pygments"
;;   - Arch Linux: "pacman -S ctags python-pygments"
(setenv "GTAGSLABEL" "pygments")
;; create tags: (choose one way)
;;   - console: "gtags --gtagslabel=pygments" (no option if set env var)
;;   - helm-gtags: =helm-gtags-create-tags= "C-c g c"

;; Frontend: /ggtags/
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook (lambda ()
;;    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;      (ggtags-mode 1))))
;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; Frontend: /helm-gtags/
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
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
;; Keybindings
;; - jumps through definitions, references, symbols or DWIM
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag) ;(definitions)
(define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag) ;(references)
(define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)  ;(symbols)
;; - show list of tags in different scopes: project, file, function
(define-key helm-gtags-mode-map (kbd "C-c g l") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-parse-file)
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; - jumping history/stacks
(define-key helm-gtags-mode-map (kbd "C-c g [") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c g ]") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)
;; - create/update tags
(define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)
(define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags)
;; (Note: prefix "C-u" update the whole project, instead of the current file)



;;
;; --------- Source Code Browsers via other Packages -------------
;;

;; Fuzzy matching for "semantic" and "Imenu" listing via HELM
;;   keybinding: "C-c h i"
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(with-eval-after-load 'helm-semantic      ;; default: c, python, elisp
  (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (push '(emacs-lisp-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (nbutlast helm-semantic-display-style 2)) ;; remove the default elisp setting
(when y:enable-cedet-semantics
  ;; enable semantic (from CEDET) support for elisp
  (add-hook 'semantic-mode-hook
            (lambda ()
              (when (fboundp 'semantic-default-elisp-setup)
                (semantic-default-elisp-setup))))
  ;; enable semantic only for c/c++, elisp (e.g. freeze python-mode)
  (semantic-mode 1)
  ;; setting GNU /global/ for /semantic-symref/
  (setq semantic-symref-tool 'global))

;; Show the function name at the first line of the current buffer via CEDET
(when y:enable-cedet-semantics
  ;; require (semantic-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (require 'stickyfunc-enhance))

;; Package: /function-args/
;; usages:
;;   =fa-show= "M-o",          =fa-jump-maybe= "M-j"
;;   =moo-jump-local= "C-M-j", =moo-jump-directory= "C-M-k"
(when y:enable-function-args
  (require 'ivy)
  (require 'function-args)
  ;; enable case-insensitive searching
  (set-default 'semantic-case-fold t)
  ;; set selection interface
  (setq moo-select-method 'helm)  ;; ivy, helm, helm-fuzzy
  ;; enable function-args
  (add-hook 'c-mode-hook 'fa-config-default)
  (add-hook 'c++-mode-hook 'fa-config-default)
  ;; semantic refresh: "M-x semantic-force-refresh"
  ;; restore default keybindings
  ;; "M-u": fa-abort; "M-o": moo-complete
  (define-key function-args-mode-map (kbd "M-u") 'upcase-word)
  (define-key function-args-mode-map (kbd "M-o") 'open-previous-line))



;; ----------- More Descriptions of helm-tags from tuhdo.github.io -------------
;; Basic concepts of tag
;; + GTAGS:  definition database
;; + GRTAGS: reference database
;; + GPATH:  path name database

;; Basic movements
;; + =forward-sexp= C-M-f:
;;   move forward over a balanced expression that can be a pair or a symbol
;; + =backward-sexp= C-M-b:
;;   move backward ...
;; + =kill-sexp= C-M-k:
;;   kill balaced expression forward that can be a pair or a symbol
;; + =mark-sexp= C-M-<SPC> or C-M-@:
;;   put mark after following expression that can be a pair or a symbol
;; + =beginning-of-defun= C-M-a:
;;   move point to beginning of a function
;; + =end-of-defun= C-M-e:
;;   move point to end of a function
;; + =mark-defun= C-M-h:
;;   put a region around whole current or following function

;; Show outline tree of definitions in current buffer via /function-args/
;;  =moo-jump-local= from /function-args/; use it as an outline tree

;; Find definitions in project, use /helm-gtags/:
;; + jump to a reference/tag definition/header
;;   =helm-gtags-dwim= M-.
;; + jump back to original location
;;   =tags-loop-continue= M-,
;; + use helm to display all available tags in a project and incrementally filtering
;;   =helm-gtags-select= C-j

;; Find references in project, use /helm-gtags/:
;; + find references to funcitons only
;;   =helm-gtags-dwim= or =helm-gtags-find-rtags= "C-c g r"
;; + find references to variables
;;   =helm-gtags-find-symbol= "C-c g s"

;; Find functions that current functions call
;; + list all the functions that the current function that point is inside calls
;;    =helm-gtags-tags-in-this-function= "C-c g a"

;; Find files in project
;; + find files matching regexp. (If point is on an included header file, =helm-gtags-dwim= automatically jumps to files)
;;   =helm-gtags-find-files=

;; View visited tags with tag stack
;; + show visited tags from newest to oldest, from top to bottom.
;;   =helm-gtags-show-stack=
