; =======================================
;; Programming Environment for /C C++/
(require 'cc-mode)
;; Warning: semantic-mode in CEDET causes "M-x gdb" hangs emacs in Mac OS X!

;; default c++-mode for .h files
;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; defaul coding styles
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

;; Package: /google-c-style/
;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Package: /smartparens/
;; having enable globally in .emacs
;; if not using /smartparens/ globally, uncomment the next line
;(require 'smartparens)
;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))

;; /flymake-google-cpplint/ (having built-in /flymake-cursor/ functionality)
; let's define a function for flymake initialization
(defun y:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command
     (if (string-equal system-type "darwin") "/usr/local/bin/cpplint"
       "/usr/bin/cpplint")))
  (flymake-google-cpplint-load))
;(add-hook 'c-mode-hook 'y:flymake-google-init)
;(add-hook 'c++-mode-hook 'y:flymake-google-init)

;; /xcscope/: source cross-referencing tool [need to install cscope]
;; (add-to-list 'load-path "~/.emacs.d/git/xcscope")
;(require 'xcscope)
;(cscope-setup)

;; configure /company-mode/ for C/C++ sources and headers
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
;; suppress warning due to set 'company-clang-arguments'
(add-to-list 'safe-local-variable-values
             '(company-clang-arguments . ("-I./include/" "-I./src/")))

;; Package: /GNU global/ + /helm-gtags/ to support tags
(load (concat y-init-path-prefix "emacs-init-cc-tags"))

;; Package: /CEDET (part)/
;; - usage: source code information
(when (string-equal y:enable-semantics "yes")
  (require 'semantic)
  (global-semantic-idle-scheduler-mode 1)
  (global-semanticdb-minor-mode 1)
  ;; setting include paths
  (semantic-add-system-include "/usr/include/c++/7.1.1" 'c++-mode)
  (semantic-add-system-include "/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include" 'c-mode)

  ;; display function interface in the minibuffer
  (global-semantic-idle-summary-mode 1)

  ;; show the function at the first line of the current buffer
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (require 'stickyfunc-enhance))

;; Package: /function-args/
;; - keybinding:
;;     fa-show =C-c M-i=; moo-complete =C-c M-o=
;;     moo-jump-local =C-M-j=; moo-jump-directory =C-M-k=
(when (string-equal y:enable-semantics "yes")
  (require 'ivy)
  (require 'function-args)
  ;; enable case-insensitive searching
  (set-default 'semantic-case-fold t)
  ;; set selection interface
  (setq moo-select-method 'ivy)  ;; ivy, helm, helm-fuzzy
  ;; enable function-args
  (add-hook 'c-mode-hook 'fa-config-default)
  (add-hook 'c++-mode-hook 'fa-config-default)
  ;; keybindings
  (define-key function-args-mode-map   (kbd "C-c M-o")  'moo-complete)
  (define-key function-args-mode-map   (kbd "C-c M-i")  'fa-show)
  ;; restore default keybindings
  (define-key function-args-mode-map (kbd "M-u") 'upcase-word)
  (define-key function-args-mode-map (kbd "M-o") 'open-previous-line))


;;
;; ***********other modes related to C/CPP ********************
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
