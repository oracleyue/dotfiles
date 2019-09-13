;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv jedi=
;;    =pip install pyflakes=
;;    =pip install autopep8=

;; If you use default pyls in LSP mode, you need to install
;;    =pip install python-language-server=
;; Alternatively, you can use Miscrosoft Python Language Server (mspyls):
;;    install "VSCode" and set its "jediEnable" to false to download mspyls.

;; Install required Emacs packages
;; (setq custom/py-packages
;;       '(dtrt-indent
;;         highlight-indent-guides))
;; (custom/install-packages custom/py-packages)
;; (if (and *use-lsp* *use-mspyls*)
;;     (custom/install-packages '(lsp-python-ms))
;;   (custom/install-packages '(jedi company-jedi)))

;; Usages:
;; *edit*
;;   - shift selected blocks "C-c >", "C-c <"
;;   - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;;
;; *source navigation*
;;   - helm-semantic-or-imenu: "C-c h i"
;;   - counsel-semantic-or-imenu: "M-g i"
;;
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;     then evaluate buffer in iPython


;; ------------------------------------------------
;; Python Environment
;; ------------------------------------------------
(use-package python
  :ensure nil
  :config
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
    (interactive (list (gud-query-cmdline
                        'python (concat "-m pdb " (file-name-nondirectory
                                                   buffer-file-name))))))

  ;; Indentation
  (defun zyue-py-indent-display-style ()
    (setq python-indent-offset 4
          tab-width 4
          python-indent-guess-indent-offset nil))
  ;; detect using tab or spaces
  (use-package dtrt-indent
    :init (add-hook 'python-mode-hook #'dtrt-indent-mode))
  ;; load tab display style
  (add-hook 'python-mode-hook #'zyue-py-indent-display-style)

  ;; Editing enhancement
  (require 'epy-editing)

  ;; Highlight indentation and current line
  (defun zyue-edit-hl-config()
    ;; highlight indentation
    (use-package highlight-indent-guides
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

  ;; Send current line to interpreter and add menu entry
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
  (easy-menu-define-key python-menu [send-line]
                        '(menu-item "Eval line" python-shell-send-line
                                    "Eval line in inferior Python session")
                        "Eval region")

  ;; Abo-abo's lpy (To-do)

) ;; End of use-package python


;; ------------------------------------------------
;; Code Auto-completion: LSP (pyls/mspyls) or Jedi
;; ------------------------------------------------
(if *use-lsp*
    ;; use "pyls" by default

    (when *use-mspyls*
      ;; use Microsoft Python Language Server for Auto-completion
      (use-package lsp-python-ms
        :demand t
        :hook (python-mode . lsp)
        :config
        ;; set Microsfot language server; installing mspyls in VSCode by disabling jedi
        (when *is-mac*   (setq vscode-path "~/.vscode/extensions "))
        (when *is-linux* (setq vscode-path "~/.vscode-oss/extensions "))
        (setq lsp-python-ms-executable
              (string-trim (shell-command-to-string
                            (concat "find " vscode-path "-name 'Microsoft.Python.LanguageServer' | tail -1"))))
        (setq lsp-python-ms-dir
              (file-name-directory lsp-python-ms-executable)))
      )

  ;; integration with /company-mode/
  (use-package company-jedi
    :config
    ;; add jedi function to menus
    (define-key python-mode-map "\C-ce" 'jedi:show-doc)
    (define-key python-mode-map "\C-c\C-e" 'jedi:get-in-function-call)
    (easy-menu-define-key python-menu [jedi-show-doc]
                          '(menu-item "Jedi show doc" jedi:show-doc
                                      "Get help on symbol at point by Jedi")
                          "Complete symbol")
    (easy-menu-define-key python-menu [jedi-call-tip]
                          '(menu-item "Jedi show calltip"
                                      jedi:get-in-function-call
                                      "Get help on function call-tip at point by Jedi") "Complete symbol")
    (easy-menu-remove-item python-mode-map '(menu-bar "Python") "Help on symbol")

    ;; setup company backends
    (defun zyue/company-py-setup ()
      (setq-local company-backends
                  (append '(company-jedi) company-backends)))
    (add-hook 'python-mode-hook 'zyue/company-py-setup))

  ) ;; end of (if *use-lsp*)



(provide 'init-python)
;; ================================================
;; init-python.el ends here
