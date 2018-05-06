;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv jedi epc argparse=
;;    =pip install pyflakes=

;; ;; Install required emacs packages
;; (setq custom/py-ac-packages
;;       '(jedi
;;         jedi-core
;;         company-jedi))
;; (custom/install-packages custom/py-ac-packages)

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
;;   - /helm-semantic-or-imenu/: "C-c h i"
;;
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;     then evaluate buffer in iPython



;; Python Major Mode /emacs-for-python/
(use-package emacs-for-python
  :load-path "~/.emacs.d/git/emacs-for-python/"
  :config
  ;; set the python interpreter
  ;; (setq python-shell-interpreter "python2") ; use python2
  (if *is-mac*
      (setq python-shell-interpreter "ipython3") ; use ipython; may slow down openning files
    (setq python-shell-interpreter "ipython2"))
  (setq python-shell-interpreter-args "--simple-prompt -i") ; fix bugs of ipython5
  ;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
  ;; (setq python-shell-interpreter-args "-i --pylab --simple-prompt --no-color-info")

  ;; set syntax checker
  (epy-setup-checker "pyflakes %f")          ; use *flymake* checker

  ;; editing settings for python
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (setq tab-width 4)
              (setq python-indent 4))))


;; Auto-completion by /Jedi/ (using /company-jedi/)
(use-package jedi
  :config
  (jedi-mode 1) ;; not necessary for company, but for code nagivation

  ;; basic settings of jedi
  (setq jedi:get-in-function-call-delay 200)  ;; set huge to disable auto show
  (setq jedi:tooltip-method nil)  ;popup, pos-tip OR nil (use minibuffer)

  ;; set virtualenv to use python2 (default: python3)
  ;; (setq jedi:environment-virtualenv
  ;;       (list "virtualenv2" "--system-site-packages"))

  ;; integration with /company-mode/
  (use-package company-jedi
    :config
    (defun y:company-py-setup ()
      (setq-local company-backends
                  (append '(company-jedi) company-backends)))
    (add-hook 'python-mode-hook 'y:company-py-setup)))



(provide 'init-py)
;; ================================================
;; init-py.el ends here
