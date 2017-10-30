
; =====================================================
;; Programming Environment for C/C++ (modern)
; =====================================================

;; check and install essential pkgs
(setq custom/modern-cc-packages
      '(irony
        company-irony
        company-irony-c-headers
        flycheck-irony
        irony-eldoc
        helm-make))
(custom/install-packages custom/modern-cc-packages)

(require 'cc-mode)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

;; /smartparens/: insert pair of symbols
;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))

;; /google-c-style/ and /flymake-google-cpplint/ style checker
(when y:enable-google-cpp-style
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  (defun y:flymake-google-init ()
    (require 'flymake-google-cpplint)
    (setq flymake-google-cpplint-command
      (if (string-equal system-type "darwin")
          "/usr/local/bin/cpplint" "/usr/bin/cpplint"))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'y:flymake-google-init)
  (add-hook 'c++-mode-hook 'y:flymake-google-init))

;; /irony/+/company-irony/: code completions
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(setq irony--server-executable (expand-file-name "~/.emacs.d/bin/irony-server"))
(add-to-list 'irony-additional-clang-options "-std=c++11")

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

(require 'company-irony-c-headers)
(defun y:add-company-backend-irony ()
  (setq-local company-backends
              (append '((company-irony-c-headers company-irony))
                      company-backends)))
(add-hook 'c-mode-hook 'y:add-company-backend-irony)
(add-hook 'c++-mode-hook 'y:add-company-backend-irony)

;; /flycheck/: syntax checker
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Compile commands in c/c++ and makefile modes
;; use helm-make
(global-set-key (kbd "C-c p c") 'helm-make-projectile)
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map
                       (kbd "C-c C-c") 'helm-make)))
;; default mode for Makefile in gnome
(add-hook 'makefile-gmake-mode-hook
          (lambda () (define-key makefile-gmake-mode-map
                       (kbd "C-c C-c") 'helm-make)))
;; default mode for Makefile in Mac OS X
(add-hook 'makefile-bsdmake-mode-hook
          (lambda () (define-key makefile-bsdmake-mode-map
                       (kbd "C-c C-c") 'helm-make)))
;; compilation setup for cmake-mode
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq compile-command "cd build/ && cmake .. && make")
            (define-key cmake-mode-map (kbd "C-c C-c") 'compile)))

(put 'helm-make-build-dir 'safe-local-variable 'stringp)

;; /cmake-mode/: cmake-mode.el
(require 'cmake-mode)
;; /cmake-font-lock/: to add more fontifying features
(add-to-list 'load-path "~/.emacs.d/git/cmake-font-lock")
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
;; adding /company-cmake/ for ac-complete
(load-file (expand-file-name "git/company-cmake.el"
                             user-emacs-directory))
(add-to-list 'company-dabbrev-code-modes 'cmake-mode)
(defun y:company-cmake-setup ()
  (setq-local company-backends
              (append '((company-cmake company-dabbrev-code))
                      company-backends)))
(add-hook 'cmake-mode-hook 'y:company-cmake-setup)

(provide 'emacs-init-cc-modern)
;; ================================================
;; emacs-init-cc-modern.el ends here
