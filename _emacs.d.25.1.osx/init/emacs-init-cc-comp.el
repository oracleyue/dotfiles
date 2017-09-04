; =====================================================
;; Programming Environment for C/C++
; =====================================================
;; Warning: semantic-mode in CEDET causes "M-x gdb" freeze emacs on OSX!
;; Features:
;;   use "C-c h i" to show symbol reference table
;;   create ".dir-local.el" to enable completion for local codes
;;   use helm-projectile to browse files in project
;;      - "C-c p a" to switch between .h, .c and .cpp
;;      - jump to "f" (file); "d" (directory); "b" (buffer); "e" (recent files)
;;      - grep in project: "C-c p g s"
;;      - multi-occur in project buffers: "C-c p o"
;;   use helm-gtags to jump via tags
;;      - use "C-c g c" create tags first and "C-c g u" to update
;;      - use "M-." and "M-," to jump and jump back (see more in "emacs-init-tags.el")


(require 'cc-mode)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

;; /smartparens/: insert pair of symbols
;; (require 'smartparens) ;; enabled in .emacs
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
    (custom-set-variables
     '(flymake-google-cpplint-command
       (if (string-equal system-type "darwin") "/usr/local/bin/cpplint"
         "/usr/bin/cpplint")))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'y:flymake-google-init)
  (add-hook 'c++-mode-hook 'y:flymake-google-init))

;; /flycheck/: syntax checker using clang
;;  - "C-c ! n" and "C-c ! p": jump to next or previous errors
;;  - "C-c ! l": list errors
;;  - "C-c ! c": menually run checker
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (flycheck-mode 1)
            (setq flycheck-clang-language-standard "c++11")))


;; /company-mode/: code completion for C/C++ sources and headers
;; -------------------BEGIN-------------------------------
(require 'company-clang)
(add-to-list 'company-clang-arguments "-I/usr/local/include")
(add-to-list 'company-clang-arguments "-I/usr/local/include/eigen3")
;; use /clang/ and /company-c-headers/
(require 'company-c-headers)
(add-to-list 'company-c-headers-path-system
             "/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0")
(add-to-list 'company-c-headers-path-system "/usr/local/include")
(add-to-list 'company-c-headers-path-system "/usr/local/include/eigen3")
;; setup backends
(defun y:company-cpp-setup ()
  (setq-local company-backends
              (append '((company-c-headers company-clang company-dabbrev-code))
                      company-backends)))
(add-hook 'c-mode-common-hook 'y:company-cpp-setup)
;; local project code completion
(put 'company-clang-arguments 'safe-local-variable #'listp)
;; =.dir-locals.el=: create it in project root and setup local inlcude paths as below
;;  ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
;;                                       "-I/home/<user>/project_root/include2/")))))
;;  one may use "dir" in yasinppet to create quickly
;; ------------------- END -------------------------------

;; /semantic-refactor/: auto generate class/function implementation
;;                      (warning: buggy for C++ template class)
;; (when y:enable-cedet-semantics
;;   (require 'srefactor)
;;   (semantic-mode 1)      ;; require semantic-mode for cc
;;   (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;   (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))



;;
;; ***********other modes support C/CPP programming ********************
;;

;; Compile commands in c/c++ and makefile modes
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in gnome
(add-hook 'makefile-gmake-mode-hook
          (lambda () (define-key makefile-gmake-mode-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in Mac OS X
(add-hook 'makefile-bsdmake-mode-hook
          (lambda () (define-key makefile-bsdmake-mode-map (kbd "C-c C-c") 'compile)))


;; Major modes for CMake files
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
