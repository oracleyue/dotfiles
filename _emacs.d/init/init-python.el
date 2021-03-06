;; ================================================================
;; Programming Environment for /Python/
;; ================================================================
;; Last modified on 06 Mar 2021

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
      (progn (setq python-shell-interpreter "ipython3")
             (setq python-shell-interpreter-args "--simple-prompt -i"))
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))
  ;; disable readline based native completion
  ;; (setq python-shell-completion-native-enable nil)

  ;; ---------------- Editing ----------------
  ;; efficient editing
  (require 'epy-editing)

  ;; tab/space detection
  (use-package dtrt-indent
    :diminish
    :hook (python-mode . dtrt-indent-mode))

  ;; indentation
  (defun zyue-py-indent-style ()
    (setq python-indent-offset 4
          tab-width 4
          python-indent-guess-indent-offset nil))
  (add-hook 'python-mode-hook #'zyue-py-indent-style)

  ;; highlight indentation
  (use-package highlight-indent-guides
    :diminish
    :init
    (setq highlight-indent-guides-method 'character) ;; 'fill, 'column
    :hook
    (python-mode . highlight-indent-guides-mode))

  ;; ---------------- Linting ----------------
  ;; lsp-mode uses "lsp" linter and sets "flycheck-checker" to disable others
  ;; nox supports "python-flake8" or "python-pylint"

  ;; ---------------- Running Interface ----------------
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
  (define-key python-mode-map (kbd "C-c C-j") 'python-shell-send-line)
  (easy-menu-define-key python-menu [send-line]
                        '(menu-item "Eval line" python-shell-send-line
                                    "Eval line in inferior Python session")
                        "Eval region")

  ;; ---------------- Auto-completion ----------------
  ;; use LSP to support: lsp-mode ("init-lsp.el") or nox ("init-nox.el")

  ;; ---------------- Debugging ----------------
  ;; use built-in GUD debugger
  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive (list (gud-query-cmdline
                        'python (concat "-m pdb " (file-name-nondirectory
                                                   buffer-file-name))))))
  ;; use DAP-based debugger (enabled in "init-lsp.el")
  ;; if not default "python", specify here:
  ;; (setq dap-python-executable "python3"))

  ) ;; End of python-mode

;; ------------------------------------------------
;; Abo-abo's lpy (To-do)
;; ------------------------------------------------
;; fix /lpy/ bug on plt.show(), which freezes emacs if not closing its window
;; (setq python-shell-interpreter-args
;;       "-i --pylab --simple-prompt --no-color-info")


(provide 'init-python)
;; ================================================
;; init-python.el ends here
