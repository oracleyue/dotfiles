; =====================================================
;; Programming Environment for C/C++
; =====================================================
;; (Warning: semantic-mode in CEDET causes "M-x gdb" freeze emacs on OSX!)

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)


;; /google-c-style/: google c++ code style
;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; /flymake-google-cpplint/ (+ built-in /flymake-cursor/)
(defun y:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command
     (if (string-equal system-type "darwin") "/usr/local/bin/cpplint"
       "/usr/bin/cpplint")))
  (flymake-google-cpplint-load))
;(add-hook 'c-mode-hook 'y:flymake-google-init)
;(add-hook 'c++-mode-hook 'y:flymake-google-init)


;; /smartparens/: insert pair of symbols
;; (require 'smartparens) ;; enabled in .emacs
;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))


;; /company-mode/: code completion for C/C++ sources and headers
(require 'company-clang)
(add-to-list 'company-clang-arguments "-I/usr/local/include/eigen3")
;; use /clang/ and /company-c-headers/
(require 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0")
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/x86_64-apple-darwin16.5.0")
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/backward")
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include")
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/include")
(add-to-list 'company-c-headers-path-system "/usr/local/include")
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include-fixed")
(add-to-list 'company-c-headers-path-system "/usr/include")
(add-to-list 'company-c-headers-path-system "/usr/local/include/eigen3")
;; setup backends
(defun y:company-cpp-setup ()
  (setq-local company-backends
              (append '((company-c-headers company-clang company-dabbrev-code))
                      company-backends)))
(add-hook 'c-mode-common-hook 'y:company-cpp-setup)




;;
;; ***********other modes support C/CPP programming ********************
;;

;; Enable compile command in Makefile modes
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in gnome
(add-hook 'makefile-gmake-mode-hook
          (lambda () (define-key makefile-gmake-mode-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in Mac OS X
(add-hook 'makefile-bsdmake-mode-hook
          (lambda () (define-key makefile-bsdmake-mode-map (kbd "C-c C-c") 'compile)))


;; Enable major modes for CMake files
;; /cmake-mode/: cmake-mode.el
(require 'cmake-mode)
;; /cmake-font-lock/: to add more fontifying features
(add-to-list 'load-path "~/.emacs.d/git/cmake-font-lock")
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
;; adding /company-cmake/ for ac-complete
(add-to-list 'company-dabbrev-code-modes 'cmake-mode)
(defun y:company-cmake-setup ()
  (setq-local company-backends
              (append '((company-cmake company-dabbrev-code))
                      company-backends)))
(add-hook 'cmake-mode-hook 'y:company-cmake-setup)


;; /doxymacs/ to manipulate doxygen documentations
;;
;; (add-to-list 'load-path "~/.emacs.d/git/doxymacs-1.8.0")
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; ; fontify the doxygen keywords
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
