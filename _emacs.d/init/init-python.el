;; ================================================================
;; Programming Environment for /Python/
;; ================================================================
;; Last modified on 04 Mar 2021

;; If you use default pyls in LSP mode, you need to install
;;    =pip install python-language-server=
;; Alternatively, you can use Miscrosoft Python Language Server (mspyls):
;;    install "VSCode.app" and set its language server to "Microsoft" to donwload mypyls

;; WARNING: a common mistake making pyls/mspyls fail to complete is due
;; to lsp auto guess root, which make your whole Home as a project root.
;; Set a project root neatly.

;; EDIT:
;; - shift selected blocks "C-c >", "C-c <"
;; - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;; SOURCE OVERVIEW: (more with "lsp-mode")
;; - counsel-semantic-or-imenu: "M-g i"
;; DEBUG: (more with "dap-mode")
;; - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;; - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;   then evaluate buffer in iPython

;; ------------------------------------------------
;; Python Environment
;; ------------------------------------------------
(use-package python
  :ensure nil
  :config
  ;; ---------------- Interpreter ----------------
  (if *use-ipython*
      (progn
        ;; using ipython may slow down openning files
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "--simple-prompt -i"))
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  ;; ---------------- Editing ----------------
  ;; indentation
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

  ;; editing enhancement
  (require 'epy-editing)

  ;; highlight indentation and current line
  (defun zyue-edit-hl-config()
    (use-package highlight-indent-guides
      :config
      (setq highlight-indent-guides-method 'character) ;; 'fill, 'column
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

  ;; ---------------- Linting ----------------
  ;; lsp-mode uses "lsp" linter and sets "flycheck-checker" to disable others
  ;; "python-flake8" or "python-pylint" are too solow

  ;; ---------------- Running Interface ----------------
  ;; send current line to interpreter and add menu entry
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

  ;; ---------------- Debugging ----------------
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
;; install: "pip install python-language-server"
(use-package lsp-pyls
  :ensure nil
  :if (eq *py-language-server* 'pyls)
  :after lsp-mode
  :hook (python-mode . lsp)
  :config
  (setq lsp-clients-python-library-directories '("/usr/local/lib/" "/usr/lib/")))

;; use Microsoft Python Language Server for Auto-completion
;; use "M-x lsp-python-ms-update-server" to upgrade from Miscrosoft
(use-package lsp-python-ms
  :if (eq *py-language-server* 'mspyls)
  :after lsp-mode
  :init
  ;; auto download released executable mspyls from Miscrosoft
  (setq lsp-python-ms-auto-install-server t
        lsp-python-ms-cache "Library")        ;; cache parsing
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

;; use Microsoft Pyright language server
;; install by "npm install -g pyright"
(use-package lsp-pyright
  :if (eq *py-language-server* 'pyright)
  :after lsp-mode
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

;; ------------------------------------------------
;; Abo-abo's lpy (To-do)
;; ------------------------------------------------
;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
;; (setq python-shell-interpreter-args
;;       "-i --pylab --simple-prompt --no-color-info")


(provide 'init-python)
;; ================================================
;; init-python.el ends here
