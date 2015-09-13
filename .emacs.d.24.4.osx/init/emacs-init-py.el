; =======================================
;; Programming Environment for /Python/

;;
;; using /emacs-for-python/
;;

;; USAGES:
;; ------------------
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts; then evaluate buffer in iPython



;; Load by default
;(load-file "~/.emacs.d/git/emacs-for-python/epy-init.el")
;; custom load
(add-to-list 'load-path "~/.emacs.d/git/emacs-for-python/")
(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
;(require 'epy-completion) ;; If you want the autocompletion settings [optional] ;; use /jedi/
(require 'epy-editing)    ;; For configurations related to editing [optional]
;(require 'epy-bindings)   ;; For my suggested keybindings [optional]
(require 'epy-nose)       ;; For nose integration

; use *IPython*
(epy-setup-ipython)

; use *flymake* checker
(epy-setup-checker "pyflakes %f")

; Edit: *indentation and line highlight*
(defun epy-edit-hl-config()
  ;; * /highlight-indentation/ in "epy-editing.el"; face configured in .emacs
  (require 'highlight-indentation)
  (highlight-indentation)
  ;; * highlight indentation by /indent-guide/  ;; not working well with popup.el
  ;(require 'indent-guide)
  ;(indent-guide-mode t)
  ;(setq indent-guide-recursive t)  ; to show all guide lines, default only one
  
  ;; * highlight line; face by default
  (hl-line-mode t))
(add-hook 'python-mode-hook 'epy-edit-hl-config)

; disabling the auto-pairing of parenthesis by /emacs-for-python/; use /smartparen/ in .emacs
(setq skeleton-pair nil)

; disabling *ropemacs*
(setq epy-enable-ropemacs nil)

; adding snippets in /emacs-for-python/ when use the default yasnippets
;; having defined in "emacs-init-ac.el" to avoid yas-reload-all again
(add-to-list 'yas-snippet-dirs "~/.emacs.d/git/emacs-for-python/extensions/yasnippet/snippets")
(yas-reload-all)

;;
;; Auto-completion by /Jedi/
;;
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)     ; optional
;;set wait time before showing funciton call signature tooltip in ms
(setq jedi:get-in-function-call-delay 200)
;; use jedi to show python doc of the object at point; =jedi:show-doc=
;; - keybinding: "C-c ?"
;; use jedi to jump to the definition of the obj at point; =jedi:goto-definition=
;; - keybinding: "C-c ."
;; go to the last point where =jedi:got-definition= was called
;; - keybinding: "C-c ,"
;; reset jediepcserver backend for osx machines
(cond
 ((string-equal system-type "darwin")
  ;; (setq jedi:server-command '("/usr/local/bin/jediepcserver"))))
  (setq jedi:server-command '("~/.emacs.d/elpa/jedi-core-20150528.2022/jediepcserver-osx.py"))))

;;; set max width to fix bug in popup menu in jedi (having set globally)
;; (defun python-mode-ac-popup-width ()
;;   (setq-local ac-max-width 0.5))
;; (add-hook 'python-mode-hook 'python-mode-ac-popup-width)
