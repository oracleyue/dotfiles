;; Programming Environment for /C C++/
;; Last modified on 29 Aug 2017

(require 'cc-mode)
;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Warning: semantic-mode in CEDET causes "M-x gdb" hangs emacs in Mac OS X!

;; coding styles
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


;; Configure /auto-complete/ for C/C++ sources and headers
;; -------------------BEGIN-------------------------------
;; /auto-complete-clang-async/: clang completion for C/C++, compiling requested
;; Note: list include directories by "gcc -xc++ -E -v -"
(cond
 ((string-equal system-type "gnu/linux")
  (add-to-list 'load-path "~/.emacs.d/git/clang-complete-async")
  (setq ac-clang-cflags    ;; for /emacs-clang-complete-async
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/../../../../include/c++/7.1.1
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/../../../../include/c++/7.1.1/x86_64-pc-linux-gnu
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/../../../../include/c++/7.1.1/backward
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include
 /usr/local/include
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include-fixed
 /usr/include
 /usr/include/eigen3
                 ")))
  ;; default local include-paths relative to projects' "src" folder
  (setq ac-clang-cflags (append ac-clang-cflags '("-I../include" "-I./include" "-I.")))
  (require 'auto-complete-clang-async)
  (setq ac-clang-complete-executable "~/.emacs.d/git/clang-complete-async/clang-complete")
  )
 ((string-equal system-type "darwin")
  ;; choose the clang-complete packages
  (setq y-clang-complete-type "clang-complete-async")
  ;; cflags setting for ac-complete-clang-async
  (setq ac-clang-cflags
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0
 /usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/x86_64-apple-darwin16.5.0
 /usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/backward
 /usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include
 /usr/local/Cellar/gcc/7.1.0/include
 /usr/local/include
 /usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include-fixed
 /usr/include
 /usr/local/include/eigen3
                 ")))
  ;; default local include-paths relative to projects' "src" folder
  (setq ac-clang-cflags (append ac-clang-cflags '("-I../include" "-I./include" "-I.")))
  ;; configuration start
  (add-to-list 'load-path "~/.emacs.d/git/clang-complete-async")
  (require 'auto-complete-clang-async)
  (setq ac-clang-complete-executable "~/.emacs.d/git/clang-complete-async/clang-complete")))

;; standard headers completion
(require 'auto-complete-c-headers) ;; setup headers completion
(cond
   ((string-equal system-type "gnu/linux")
    (add-to-list 'achead:include-directories '"/usr/include")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/backward")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/x86_64-unknown-linux-gnu")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include-fixed")
    (add-to-list 'achead:include-directories '"/usr/include/eigen3"))
   ((string-equal system-type "darwin")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/x86_64-apple-darwin16.5.0")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/include/c++/7.1.0/backward")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/include")
    (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/7.1.0/lib/gcc/7/gcc/x86_64-apple-darwin16.5.0/7.1.0/include-fixed")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/usr/include")
    (add-to-list 'achead:include-directories '"/usr/local/include/eigen3")
    (ac-clang-launch-completion-process))
   )
;; setup ac-complete (sources & headers)
(defun y:ac-clang-config ()
  (setq ac-clang-async-do-autocompletion-automatically nil) ; disable auto-trigger
  (setq ac-sources '(ac-source-clang-async
                    ;ac-source-semantic
                     ac-source-c-headers))
  (ac-clang-launch-completion-process))
(add-hook 'c-mode-hook 'y:ac-clang-config)
(add-hook 'c++-mode-hook 'y:ac-clang-config)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; ------------------------END----------------------------


;; ---------------- Other Major Modes for C/C++ Supportings ----------------

;; compilation shortcuts
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in gnome
(add-hook 'makefile-gmake-mode-hook
          (lambda () (define-key makefile-gmake-mode-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in Mac OS X
(add-hook 'makefile-bsdmake-mode-hook
          (lambda () (define-key makefile-bsdmake-mode-map (kbd "C-c C-c") 'compile)))

;; /cmake-mode/: major mode for CMake files
(require 'cmake-mode)
;; /cmake-font-lock/: to add more fontifying features
(add-to-list 'load-path "~/.emacs.d/git/cmake-font-lock")
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; /doxymacs/ to manipulate doxygen documentations
;; (add-to-list 'load-path "~/.emacs.d/git/doxymacs-1.8.0")
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; ; fontify the doxygen keywords
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)