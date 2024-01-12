;; ================================================================
;; Programming Environment for /Python/
;; ================================================================
;; Last modified on 06 Mar 2021

;; Install LSP langserver, like pyls, mspyls, pyright (default)

;; EDIT:
;; - shift selected blocks "C-c >", "C-c <"
;; - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;;
;; SOURCE OVERVIEW: (more with "lsp-mode")
;; - counsel-semantic-or-imenu: "M-g i"
;;
;; DEBUG: (more with "dap-mode")
;; - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;; - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;   then evaluate buffer in iPython
;;
;; VENV:
;; - "M-x pyvenv-workon" to activate miniconda envs, necessary for LSP well-working

;; ------------------------------------------------
;; Python Environment
;; ------------------------------------------------
(use-package python
  :ensure nil
  :config
  ;; ---------------- Interpreter ----------------
  ;; If use virtualenv, controlled by /pyvenv/
  ;; (setq python-shell-interpreter "python"
  ;;       python-shell-interpreter-args "-i")

  ;; ------------------ Editing ------------------
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

  ;; highlight indentation (buggy)
  (use-package highlight-indent-guides
    :disabled
    :diminish
    :init (setq highlight-indent-guides-method 'character) ;; 'fill, 'column
    :hook (python-mode . highlight-indent-guides-mode))

  ;; ---------------- Linting ----------------
  ;; lsp-mode uses "lsp" linter and sets "flycheck-checker" to disable others

  ;; ---------------- Virtual Environments ----------------
  (use-package pyvenv
    :ensure t
    :bind (:map python-mode-map
                ("M-s C-a"  .  pyvenv-activate)  ;; venv at current folder
                ("M-s C-d"  .  pyvenv-deactivate)
                ("M-s C-e"  .  pyvenv-workon))   ;; select venv
    :config
    (setenv "WORKON_HOME" "~/miniconda/envs")
    (pyvenv-mode 1))

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
  ;; use LSP to support: lsp-mode ("init-lsp.el") or lsp-bridge ("init-lsp-bridge.el")

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
