;; ================================================================
;; Programming Environment for /Python/
;; ================================================================

;; Install required python packages
;;    =pip install virtualenv=
;;    =pip install pyflakes=
;;    =pip install autopep8=

;; If you use default pyls in LSP mode, you need to install
;;    =pip install python-language-server=
;; Alternatively, you can use Miscrosoft Python Language Server (mspyls):
;;    install "VSCode" and set its "jediEnable" to false to enable downloading mspyls.

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
  :hook (python-mode . lsp)
  :config
  (if *use-ipython*
      (progn
        ;; using ipython may slow down openning files
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "--simple-prompt -i"))
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  ;; Indentation
  (defun zyue-py-indent-display-style ()
    (setq python-indent-offset 4
          tab-width 4
          python-indent-guess-indent-offset nil))
  ;; detect using tab or spaces
  (use-package dtrt-indent
    :diminish
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
                             "#B8C5DB")))
      (highlight-indent-guides-mode))
    ;; highlight current line (enabled globally in "init-basics.el")
    ;; (hl-line-mode t)
    )
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

  ;; Debugging Supports
  ;; use built-in GUD debugger
  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive (list (gud-query-cmdline
                        'python (concat "-m pdb " (file-name-nondirectory
                                                   buffer-file-name))))))
  ;; use DAP-based debugger (enabled in "init-lsp.el")
  ;; if need to set specific python:
  ;; (setq dap-python-executable "python3"))

  ) ;; End of python-mode

;; ------------------------------------------------
;; Auto-completion via LSP (pyls/mspyls)
;; ------------------------------------------------
;; LSP use "pyls" by default
(eval-after-load "lsp"
  '(setq lsp-clients-python-library-directories
         '("/usr/local/lib/")))

;; use Microsoft Python Language Server for Auto-completion
(when *use-mspyls*
  (use-package lsp-python-ms
    :after lsp-mode
    :config
    ;; set mspyls that is built by vscode.app (open any .py file in vscode)
    (when *is-mac*   (setq vscode-path "~/.vscode/extensions "))
    (when *is-linux* (setq vscode-path "~/.vscode-oss/extensions "))
    (setq lsp-python-ms-executable
          (string-trim (shell-command-to-string
                        (concat "find " vscode-path "-name 'Microsoft.Python.LanguageServer' | tail -1"))))
    (setq lsp-python-ms-dir
          (file-name-directory lsp-python-ms-executable))))

;; ------------------------------------------------
;; Abo-abo's lpy (To-do)
;; ------------------------------------------------
;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
;; (setq python-shell-interpreter-args
;;       "-i --pylab --simple-prompt --no-color-info")


(provide 'init-python)
;; ================================================
;; init-python.el ends here
