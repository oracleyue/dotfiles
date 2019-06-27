;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv jedi epc argparse=
;;    =pip install pyflakes=

;; Install required Emacs packages
(setq custom/py-packages
      '(jedi
        company-jedi
        ein
        dtrt-indent))
(custom/install-packages custom/py-packages)

;; Usages:
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


;; ----------------------------------------------------------------
;; Python Major Mode /emacs-for-python/
;; ----------------------------------------------------------------
(use-package epy-init
  :load-path "~/.emacs.d/git/emacs-for-python/"
  :config
  ;; set the python interpreter
  (if (eq *use-python-version* 3)
      (setq python-shell-interpreter "ipython3") ; use ipython; may slow down openning files
    (setq python-shell-interpreter "ipython2")) ; or "python2"
  (setq python-shell-interpreter-args "--simple-prompt -i") ; fix bugs of ipython5
  ;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
  ;; (setq python-shell-interpreter-args
  ;;       "-i --pylab --simple-prompt --no-color-info")

  ;; set syntax checker
  (epy-setup-checker "pyflakes %f")          ; use *flymake* checker

  ;; set gud debugger
  ;; (setq gud-pdb-command-name "python -m pdb")
  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive
     (list (gud-query-cmdline 'python
                              (concat "-m pdb "
	 		                          (file-name-nondirectory buffer-file-name))))))

  ;; indenting
  (defun zyue:py-indent-display-style ()
    (setq python-indent-offset 4
          tab-width 4
          python-indent-guess-indent-offset nil))
  ;; detect using tab or spaces
  (use-package dtrt-indent
    :init
    (add-hook 'python-mode-hook #'dtrt-indent-mode))
  ;; load tab display style
  (add-hook 'python-mode-hook #'zyue:py-indent-display-style)

  ;; supplements to python.el
  (defun python-shell-send-line (&optional beg end)
    (interactive)
    (let ((beg (cond (beg beg)
                     ((region-active-p)
                      (region-beginning))
                     (t (line-beginning-position))))
          (end (cond (end end)
                     ((region-active-p)
                      (copy-marker (region-end)))
                     (t (line-end-position)))))
      (python-shell-send-region beg end)))
  (define-key python-mode-map "\C-c\C-j" 'python-shell-send-line)
  ;; add menu entry
  (easy-menu-define-key python-menu [send-line]
                        '(menu-item "Eval line" python-shell-send-line
                                    "Eval line in inferior Python session")
                        "Eval region")
  );;END: use-package (emacs-for-python)

;; ----------------------------------------------------------------
;; Auto-completion by /Jedi/ (using /company-jedi/)
;; ----------------------------------------------------------------
(use-package jedi
  :config
  (jedi-mode 1) ;; not necessary for company, but for code nagivation

  ;; basic settings of jedi
  (setq jedi:get-in-function-call-delay 200)  ;; set huge to disable auto show
  (setq jedi:tooltip-method nil)  ;popup, pos-tip OR nil (use minibuffer)
  (define-key python-mode-map "\C-ce" 'jedi:show-doc)
  (define-key python-mode-map "\C-c\C-e" 'jedi:get-in-function-call)
  ;; add menu entry
  (easy-menu-define-key python-menu [jedi-show-doc]
                        '(menu-item "Jedi show doc" jedi:show-doc
                                    "Get help on symbol at point by Jedi")
                        "Complete symbol")
  (easy-menu-define-key python-menu [jedi-call-tip]
                        '(menu-item "Jedi show calltip"
                                    jedi:get-in-function-call
                                    "Get help on function call-tip at point by Jedi") "Complete symbol")
  (easy-menu-remove-item python-mode-map '(menu-bar "Python") "Help on symbol")

  ;; set virtualenv to use python2 (default: python3)
  (when (eq *use-python-version* 2)
    (setq jedi:environment-virtualenv
          (list "virtualenv2" "--system-site-packages")))

  ;; integration with /company-mode/
  (use-package company-jedi
    :config
    (defun zyue/company-py-setup ()
      (setq-local company-backends
                  (append '(company-jedi) company-backends)))
    (add-hook 'python-mode-hook 'zyue/company-py-setup))

  ;; integration with /ivy/
  (when *use-ivy*
    (require 'counsel)
    (define-key python-mode-map (kbd "C-M-i") 'counsel-jedi))

  ) ;; end of use-package(jedi)

;; ----------------------------------------------------------------
;; Emacs suuport for Jupyter Notebook
;; ----------------------------------------------------------------
;; Usage:
;; start the Jupyter notebook server by call =M-x ein:jupyter-server-start=

;; (use-package ein
;;   :config
;;   (require 'ein)
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages))



(provide 'init-py)
;; ================================================
;; init-py.el ends here
