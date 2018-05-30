; =====================================================
;; Programming Environment for C/C++ (modern)
; =====================================================

(require 'cc-mode)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

;; /smartparens/: insert pair of symbols
;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))

;; /google-c-style/ and /flymake-google-cpplint/ style checker
(when *enable-gg-cpp-style*
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  (defun zyue/flymake-google-init ()
    (require 'flymake-google-cpplint)
    (setq flymake-google-cpplint-command
      (if (string-equal system-type "darwin")
          "/usr/local/bin/cpplint" "/usr/bin/cpplint"))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'zyue/flymake-google-init)
  (add-hook 'c++-mode-hook 'zyue/flymake-google-init))

(when *enable-rtags*
  ;; see the const *enable-rtags* defined in "init-features.el"
  (use-package rtags
    :config
    ;; run rtags server automatically
    (rtags-start-process-unless-running)
    (rtags-enable-standard-keybindings)
    ;; use rtags for code completions (not recommended)
    ;; (setq rtags-autostart-diagnostics t)
    ;; (setq rtags-completions-enabled t)
    ;; (use-package company-rtags
    ;;   :config
    ;;   (push 'company-rtags company-backends))
    ;; integration with helm
    (when *use-helm*
      (setq rtags-display-result-backend 'helm))))

;; /irony/+/company-irony/: code completions
(use-package irony
  :bind (:map irony-mode-map
              ("C-c C-b" . irony-cdb-menu)
              ("C-c =" . irony-get-type))
  :after cc-mode
  :config
  (setq irony--server-executable (expand-file-name
                                    "~/.emacs.d/bin/irony-server"))
  (add-to-list 'irony-additional-clang-options "-std=c++11")
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))

  (use-package company-irony-c-headers
    :config
    (defun zyue/add-company-backend-irony ()
      (setq-local company-backends
                  (append '((company-irony-c-headers company-irony))
                          company-backends)))
    (add-hook 'c-mode-hook 'zyue/add-company-backend-irony)
    (add-hook 'c++-mode-hook 'zyue/add-company-backend-irony)))

;; /flycheck/: syntax checker
(use-package flycheck
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

;; /flycheck-irony/ using /irony/
(use-package flycheck-irony
  :requires flycheck
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; /function-args/: C/C++ symbol reference tables
;; usages:
;;   =moo-jump-local= "C-M-j", =moo-jump-directory= "C-M-k"
(when (and *enable-function-args* *enable-semantics*)
  (use-package function-args
    :requires ivy
    :config
    ;; enable case-insensitive searching
    (set-default 'semantic-case-fold t)
    ;; set selection interface
    (if *use-helm*
        (setq moo-select-method 'helm)  ;; ivy, helm, helm-fuzzy
      (setq moo-select-method 'ivy))
    ;; enable function-args
    (add-hook 'c-mode-hook 'fa-config-default)
    (add-hook 'c++-mode-hook 'fa-config-default)
    ;; semantic refresh: "M-x semantic-force-refresh"
    ;; restore default keybindings
    ;; "M-u": fa-abort; "M-o": moo-complete
    (define-key function-args-mode-map (kbd "M-u") 'upcase-word)
    (define-key function-args-mode-map (kbd "M-o") 'open-previous-line)))

(use-package cmake-mode
  ;; /cmake-mode/: cmake-mode.el
  :defer t
  :config
  ;; /cmake-font-lock/: to add more fontifying features
  (use-package cmake-font-lock
    :load-path "~/.emacs.d/git/cmake-font-lock"
    :config
    (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
  ;; adding /company-cmake/ for ac-complete
  (add-to-list 'company-dabbrev-code-modes 'cmake-mode)
  (defun zyue/company-cmake-setup ()
    (setq-local company-backends
                (append '((company-cmake company-dabbrev-code))
                        company-backends)))
  (add-hook 'cmake-mode-hook 'zyue/company-cmake-setup)
  ;; compilation setup for cmake-mode
  (add-hook 'cmake-mode-hook
            (lambda ()
              (setq compile-command "cd build/ && cmake .. && make")
              (define-key cmake-mode-map (kbd "C-c C-c") 'compile))))

;; Compile commands in c/c++ and makefile modes using helm-make
(use-package helm-make
  :bind (("C-c p c" . helm-make-projectile)
         :map c-mode-base-map
         ("C-c C-c" . helm-make)
         :map makefile-gmake-mode-map   ;; makefile in Linux
         ("C-c C-c" . helm-make)
         :map makefile-bsdmake-mode-map ;; makefile in BSD
         ("C-c C-c" . helm-make)))

(provide 'init-cc)
;; ================================================
;; init-cc.el ends here
