;; ========================================
;; Settings for tags support in C/C++ programming

;; Package: /ggtags/
;(require 'ggtags)
;(add-hook 'c-mode-common-hook (lambda ()
;            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode) (ggtags-mode 1))))
;(setq-local imenu-create-index-function #'ggtags-build-imenu-index)


;; Package: GNU global (Unix) + /helm-gtags/
;; Prefix config
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 ;helm-gtags-suggested-key-mapping t
 helm-gtags-auto-update t
 ;helm-gtags-cache-select-result t
 )
(require 'helm-gtags)
;; Enable helm-gtags-mode
;(add-hook 'dired-mode-hook 'helm-gtags-mode)
;(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
;(add-hook 'asm-mode-hook 'helm-gtags-mode)
;; Keybindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c g l") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)
(define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags)
(define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-parse-file)
;(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; in the list of helm-gtags-suggested-key-mapping
(define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
(define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)


;;; ------------------------- Usages -------------------------------
;;; + run =gtags= under your project root before use
;; - a DEFINITION of a tag is where a tag is implemented
;; - a REFERENCE of a tag is where a tag is used in a source tree, but not where it is defined
;;
;;; Basic concepts of tag
;; + GTAGS: definition database
;; + GRTAGS: reference database
;; + GPATH: path name database
;;
;;; Find definitions in project, use /helm-gtags/:
;; + =helm-gtags-dwim= M-.:
;;   jump to a reference/tag definition/header
;; + =tags-loop-continue= M-,:
;;   jump back to original location
;; + =helm-gtags-select= C-j:
;;   use helm to display all available tags in a project and incrementally filtering
;;; Find references in project, use /helm-gtags/:
;; + =helm-gtags-dwim= or =helm-gtags-find-rtags= "C-c g r"
;;    find references to funcitons only
;; + =helm-gtags-find-symbol= "C-c g s"
;;    find references to variables
;;
;;; Find functions that current functions call
;; + =helm-gtags-tags-in-this-function= "C-c g a"
;;    list all the functions that the current function - the function that point is inside - calls
;;; Find files in project
;; + =helm-gtags-find-files=
;;    find files matching regexp. If point is on an included header file, =helm-gtags-dwim= automatically jumps to files
;;
;;; View visited tags with tag stack
;; + =helm-gtags-show-stack=
;;    show visited tags from newest to oldest, from top to bottom.

;; ---------------------------
;; + CEDET semantic symref (symbol references)
;;   - configuration: in "emacs-init-helm" with /semantic/
;;   - mostly only valid for small/medium project; use /helm-gtags/ for big/huge ones
;;     =semantic-symref-symbol sym=   "C-,-g"
;;     =semantic-symref=              "C-,-G"
;;   - in the buffer:
;;     + "C-c C-e" semantic-symref-list-expand-all
;;     + "C-c C-r" semantic-symref-list-contract-all
;;     + "Tab", "n", "p"     forward-butoon
;;     + "Enter"   jump to codes
;;     + "+"       semantic-symref-list-toggle-showing
;;     + "R"       semantic-symref-list-rename-open-hits
;; + /function-args/: find definitions in current buffer:
;;   - use =moo-jump-local= from /function-args/; use it as an outline tree
