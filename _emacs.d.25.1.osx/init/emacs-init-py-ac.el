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
;;     - show call-tip: "C-c e"; show doc: "C-c C-e" (keybindings by oracleyue)
;;     - use jedi to show python doc of the object at point; =jedi:show-doc=
;;       keybinding: "C-c ?"
;;     - use jedi to jump to the definition of the obj at point; =jedi:goto-definition=
;;       keybinding: "C-c ."
;;     - go to the last point where =jedi:got-definition= was called
;;       keybinding: "C-c ,"
;;   - /jedi-direx/: source viewer, "C-c v"
;;   - /helm-semantic-or-imenu/: "C-c h i"
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
(require 'epy-editing)         ;; editing [optional]
(require 'epy-bindings)        ;; suggested keybindings [optional]
;(require 'epy-nose)            ;; nose integration

(setq python-shell-interpreter "python2") ; use python
;; (setq python-shell-interpreter "ipython2") ; use *ipython*
;; (setq python-shell-interpreter-args "--simple-prompt -i") ;fix bugs of ipython5
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
;; disable trigger completion menu automatically
(setq jedi:complete-on-dot t)  ;; if nil, use "C-<tab>" on dot; else "<tab>".
;; set wait time before showing funciton call signature tip in ms
(setq jedi:get-in-function-call-delay 800)  ;; set huge to disable auto show
;; set calltip methods
(setq jedi:tooltip-method '(popup))  ;popup, pos-tip OR nil (use minibuffer)
;; restore jedi:complete bound default in jedi
(define-key python-mode-map (kbd "C-<tab>") 'jedi:complete)
;; remove ac-source-yasnippet
(add-hook 'python-mode-hook (lambda ()
   (setq ac-sources '(ac-source-jedi-direct
                      ac-source-words-in-same-mode-buffers))))

;; source code viewer via /jedi-direx/ (require /direx/ in .emacs)
(when (cdr (assoc "direx-jedi" y:use-direx-or-neotree))
  (eval-after-load "python"
    '(define-key python-mode-map "\C-cv" 'jedi-direx:pop-to-buffer))
  (add-hook 'jedi-mode-hook 'jedi-direx:setup))


(provide 'emacs-init-py-ac)
;; ================================================
;; emacs-init-py-ac.el ends here