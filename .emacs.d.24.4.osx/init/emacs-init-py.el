; =======================================
;; Programming Environment for /Python/
; using /emacs-for-python/
(load-file "~/.emacs.d/git/emacs-for-python/epy-init.el")

; use *IPython*
(epy-setup-ipython)

; use *flymake* checker
(epy-setup-checker "pyflakes %f")

; Edit: *indentation and line highlight*
(defun epy-edit-hl-config()
  ;; setting in "epy-editing.el" by /emacs-for-python/ use /highlight-indentation/
  ;; * highlight indentation by /indent-guide/  ;; not working well with popup.el
  ;(require 'indent-guide)
  ;(indent-guide-mode t)
  ;(setq indent-guide-recursive t)  ; to show all guide lines, default only one
  ;; * highlight line; the face for hl-line-mode has been set globally
  (hl-line-mode t))
(add-hook 'python-mode-hook 'epy-edit-hl-config)

; disabling *ropemacs*
(setq epy-enable-ropemacs nil)

; Auto-completion by /Jedi/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)     ; optional
;;set wait time before showing funciton call signature tooltip in ms
(setq jedi:get-in-function-call-delay 200)
;; use jedi to show python doc of the object at point
;; - keybinding: "C-c ?"
;; use jedi to jump to the definition of the obj at point
;; - keybinding: "C-c ."
;; reset jediepcserver backend for osx machines
(cond
 ((string-equal system-type "darwin")
  ;; (setq jedi:server-command '("/usr/local/bin/jediepcserver"))))
  (setq jedi:server-command '("~/.emacs.d/elpa/jedi-core-20150528.2022/jediepcserver-osx.py"))))


;;; set max width to fix bug in popup menu in jedi (having set globally)
;; (defun python-mode-ac-popup-width ()
;;   (setq-local ac-max-width 0.5))
;; (add-hook 'python-mode-hook 'python-mode-ac-popup-width)
