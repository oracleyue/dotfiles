;; =====================================================
;; Programming Environment for C/C++ (using LSP)
;; =====================================================
;; Last modified on 27 Feb 2021

;; Dependencies:
;; - clangd: lsp C++ server, install by "brew install llvm"
;; - cmake, bear: generating build flags, install by brew
;;
;; Generating "compile_commands.json":
;; - cmake-based projects:
;;   enable by "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
;; - other build systems, use Bear:
;;   run "make clean", then "bear -- make" to generate it.

(use-package cc-mode
  :ensure nil
  :after lsp-mode
  :defines (lsp-clients-clangd-executable lsp-clients-clangd-args)
  :bind (:map c-mode-base-map
              ("C-c C-c" . compile))
  :hook ((c-mode . lsp) (c++-mode . lsp))
  :init (setq-default c-default-style "linux"  ;; "stroustrup"
                      c-basic-offset 4)
  :config
  ;; code fontify
  (use-package modern-cpp-font-lock
    :diminish (modern-c++-font-lock-mode)
    :init (modern-c++-font-lock-global-mode t))

  ;; lsp with clangd
  (if *is-mac*
      (defconst clangd-bin "/usr/local/opt/llvm/bin/clangd")
    (defconst clangd-bin "/usr/bin/clangd"))
  (defconst clangd-args '("-j=2"
                          "--background-index"
                          "--clang-tidy"
                          "--recovery-ast"
                          "--cross-file-rename"
                          "--completion-style=bundled"
                          "--pch-storage=memory"
                          "--suggest-missing-includes"
                          "--header-insertion=iwyu"
                          "--header-insertion-decorators"))
  ;; set clangd-args to accelerate clangd parsing
  (with-eval-after-load 'lsp-mode
    (setq lsp-clients-clangd-executable clangd-bin
          lsp-clients-clangd-args clangd-args)))

;; ================================================
;; Other editing supports
;; ================================================

;; /smartparens/: insert pair of symbols
;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))

;; ================================================
;; CMake mode
;; ================================================
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  ;; /cmake-font-lock/: better fontifying
  (use-package cmake-font-lock
    :hook (cmake-mode . cmake-font-lock-activate))

  ;; auto completion
  (add-to-list 'company-dabbrev-code-modes 'cmake-mode)
  (defun zyue/company-cmake-setup ()
    (setq-local company-backends
                (append '((company-cmake company-dabbrev-code))
                        company-backends)))

  ;; compile setup
  (defun zyue/link-compile-commands-json (buffer msg)
    "Linking compile_commands.json to project root for LSP clangd."
    (let ((link-status nil))
      (when (string-match "^finished" msg)
        (shell-command "cd ..; ln -s build/compile_commands.json .")
        (setq link-status t))
      (with-current-buffer (get-buffer "*compilation*")
        (goto-char (point-max))
        (if link-status
            (insert "\nLinking compile_commands.json for LSP: Done :-) \n")
          (ding)
          (insert "\nLinking compile_commands.json for LSP: Failed :-( \n")))))

  (defun zyue/cmake-setup-compile ()
    (setq compile-command "cd build/ && cmake .. && make -k")
    (add-hook 'compilation-finish-functions 'zyue/link-compile-commands-json)
    (define-key cmake-mode-map (kbd "C-c C-c") 'compile))

  (defun clean-all ()
    (interactive)
    (when (file-directory-p "build")
      (delete-directory "build" t) (delete-file "compile_commands.json")
      (make-directory "build")))

  :hook ((cmake-mode . zyue/company-cmake-setup)
         (cmake-mode . zyue/cmake-setup-compile)))


(provide 'init-cc)
;; ================================================
;; init-cc.el ends here
