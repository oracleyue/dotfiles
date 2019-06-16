;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv jedi epc argparse=
;;    =pip install pyflakes=

;; Install required Emacs packages
(setq custom/py-packages
      '(dtrt-indent))
(custom/install-packages custom/py-packages)

;; Usages:
;; *edit*
;;   - shift selected blocks "C-c >", "C-c <"
;;   - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;;
;; *source navigation*
;;   - /helm-semantic-or-imenu/: "C-c h i"
;;
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;     then evaluate buffer in iPython


;; ------------------------------------------------
;; Environment Configurations
;; ------------------------------------------------
(require 'python)

;; Set interpreter
(if (eq *use-python-version* 3)
    (setq python-shell-interpreter "ipython3") ; use ipython; may slow down openning files
  (setq python-shell-interpreter "ipython2")) ; or "python2"
(setq python-shell-interpreter-args "--simple-prompt -i") ; fix bugs of ipython5
;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
;; (setq python-shell-interpreter-args
;;       "-i --pylab --simple-prompt --no-color-info")

;; Set GUD debugger
;; (setq gud-pdb-command-name "python -m pdb")
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'python
                            (concat "-m pdb "
	 		                        (file-name-nondirectory buffer-file-name))))))

;; Indentation
(defun zyue-py-indent-display-style ()
  (setq tab-width 4
        python-indent-offset 4
        python-indent-guess-indent-offset nil))
(add-hook 'python-mode-hook #'zyue-py-indent-display-style)
;; detect using tab or spaces
(use-package dtrt-indent
  :init
  (add-hook 'python-mode-hook #'dtrt-indent-mode))

;; ------------------------------------------------
;; Editing Enhancement and Highlight
;; ------------------------------------------------
(require 'epy-editing)

;; Highlight indentation and current line
(defun zyue-edit-hl-config()
  ;; highlight indentation
  (use-package highlight-indent-guides
    :ensure t
    :config
    (setq highlight-indent-guides-method 'character) ;; 'fill, 'column
    ;; tweak colors
    (cond
     ((eq zyue-theme 'doom-one)
      (setq highlight-indent-guides-auto-enabled nil)
      (set-face-foreground 'highlight-indent-guides-character-face
                           "#3e6a44a85124"))
     ((eq zyue-theme 'doom-nord-light)
      (setq highlight-indent-guides-auto-enabled nil)
      (set-face-foreground 'highlight-indent-guides-character-face
                           "#B8C5DB"))
     )
    (highlight-indent-guides-mode))
  ;; highlight current line
  (hl-line-mode t))
(add-hook 'python-mode-hook 'zyue-edit-hl-config)

;; ------------------------------------------------
;; Interface (Menu)
;; ------------------------------------------------

;; Evaluate one line
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

;; ------------------------------------------------
;; Abo-abo's lpy (To-do)
;; ------------------------------------------------



(provide 'init-python)
;; ================================================
;; init-python.el ends here
