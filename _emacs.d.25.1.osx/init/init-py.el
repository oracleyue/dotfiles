;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv jedi epc argparse=
;;    =pip install pyflakes=

;; Install required emacs packages
(setq custom/py-ac-packages
      '(jedi
        jedi-core
        company-jedi
        jedi-direx))
(custom/install-packages custom/py-ac-packages)

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

;; (setq python-shell-interpreter "python2") ; use python2
(if *is-mac*
    (setq python-shell-interpreter "ipython3") ; use ipython; may slow down openning files
  (setq python-shell-interpreter "ipython2"))
(setq python-shell-interpreter-args "--simple-prompt -i") ; fix bugs of ipython5
;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
;; (setq python-shell-interpreter-args "-i --pylab --simple-prompt --no-color-info")
(epy-setup-checker "pyflakes %f")          ; use *flymake* checker


;;
;; Additional Editing Settings
;;
(add-hook 'python-mode-hook
          (lambda ()
		    (setq indent-tabs-mode t)
		    (setq tab-width 4)
			(setq python-indent 4)))


;;
;; Auto-completion by /Jedi/ (using /company-jedi/)
;;

;; basic settings of jedi
(setq jedi:get-in-function-call-delay 200)  ;; set huge to disable auto show
(setq jedi:tooltip-method nil)  ;popup, pos-tip OR nil (use minibuffer)

;; set virtualenv to use python2 (default: python3)
;; (setq jedi:environment-virtualenv
;;       (list "virtualenv2" "--system-site-packages"))

;; integration with /company-mode/
(defun y:company-py-setup ()
  (jedi-mode 1) ;; not necessary for company, but for code nagivation and direx
  (setq-local company-backends
              (append '(company-jedi) company-backends)))
(add-hook 'python-mode-hook 'y:company-py-setup)

;; source code viewer via /jedi-direx/ (require /direx/ in .emacs)
(when (cdr (assoc "direx-jedi" y:use-direx-or-neotree))
  (eval-after-load "python"
    '(define-key python-mode-map "\C-cv" 'jedi-direx:pop-to-buffer))
  (add-hook 'jedi-mode-hook 'jedi-direx:setup))


(provide 'init-py)
;; ================================================
;; init-py.el ends here
