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
  )

;; ------------------------------------------------
;; Other editing supports
;; ------------------------------------------------

;; /smartparens/: insert pair of symbols
;; when you press RET, the curly braces automatically add another newline
(with-eval-after-load "smartparens"
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET")))))

;; CMake supports
(require 'init-cmake)


(provide 'init-cc)
;; ================================================
;; init-cc.el ends here
