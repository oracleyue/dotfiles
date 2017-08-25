; =======================================
;; Programming Environment for /Python/

;; usages
;; ------------------
;; *edit*
;;   - shift selected blocks "C-c >", "C-c <"
;;   - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;;
;; *source navigation*
;;   - /jedi/ usages:
;;     - use jedi to show python doc of the object at point; =jedi:show-doc=
;;       keybinding: "C-c ?"
;;     - use jedi to jump to the definition of the obj at point; =jedi:goto-definition=
;;       keybinding: "C-c ."
;;     - go to the last point where =jedi:got-definition= was called
;;       keybinding: "C-c ,"
;;   - /jedi-direx/: source viewer, "C-c v"
;;
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;     then evaluate buffer in iPython



;;
;; Python Major Mode /emacs-for-python/
;;

(add-to-list 'load-path "~/.emacs.d/git/emacs-for-python/")  ;; "epy-init" load all
(require 'epy-setup)           ;; required!
(setq epy-enable-ropemacs nil) ;; disabling *ropemacs* (set before "epy-python")
(require 'epy-python)          ;; python facilities [optional]
(setq epy-load-yasnippet-p t)  ;; additional snippets (set before "epy-editing")
(require 'epy-editing)         ;; editing [optional]
(require 'epy-bindings)        ;; suggested keybindings [optional]
;;(require 'epy-completion)    ;; disabled; use /jedi/ instead
(require 'epy-nose)            ;; nose integration

;(setq python-shell-interpreter "python2") ; use python
(epy-setup-ipython)                        ; use *ipython*
(epy-setup-checker "pyflakes %f")          ; use *flymake* checker

; fix bugs in ac due to line wrap
(add-hook 'python-mode-hook (lambda() (setq truncate-lines t)))

; fix <tab> completion in ipython when use auto-complete
(define-key inferior-python-mode-map (kbd "<tab>")
  'python-shell-completion-complete-or-indent)



;;
;; Auto-completion by /Jedi/
;;
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)     ; optional
;; set wait time before showing funciton call signature tip in ms
(setq jedi:get-in-function-call-delay 200)
;; set calltip methods
(setq jedi:tooltip-method '(popup))  ;popup, pos-tip OR nil (use minibuffer)

;; source code viewer via /jedi-direx/
(eval-after-load "python"
  '(define-key python-mode-map "\C-cv" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)