; =======================================
;; Programming Environment for /C C++/
(require 'cc-mode)
;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Warning: semantic-mode in CEDET causes "M-x gdb" hangs emacs in Mac OS X!

;; coding styles
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

;; Package: /flymake-google-cpplint/ (having built-in /flymake-cursor/ functionality)
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

;; Package: /xcscope/ source cross-referencing tool [need to install cscope]
;; (add-to-list 'load-path "~/.emacs.d/git/xcscope")
;(require 'xcscope)
;(cscope-setup)


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
  ;; cflags/flags setting for ac-complete-clang-async/..-clang
  (setq ac-clang-cflags       ;; for /emacs-clang-complete-async
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
  (cond
   ((string-equal y-clang-complete-type "clang-complete-async")
    ;; use /emacs-clang-complete-async/
    (add-to-list 'load-path "~/.emacs.d/git/clang-complete-async")
    (require 'auto-complete-clang-async)
    (setq ac-clang-complete-executable "~/.emacs.d/git/clang-complete-async/clang-complete")
    )
   ((string-equal y-clang-complete-type "clang-complete")
    ;; use /emacs-clang-complete/
    (require 'auto-complete-clang)
    (setq ac-clang-flags ac-clang-cflags)  ;; copy cflags from -async to clang-complete
    )
   )
  ;; LIBRARYIES: openmpi, gsl, Eigen, boost, opengl; followed by "/", e.g., <gsl/gsl_math.h>
  ;;
  ;; /usr/local/include/c++-xcode/v1
  ;; /usr/local/include/c/sys
  ;; /usr/local/include/clang
  ;
  ;; adding for cflags for clang:
  ;(setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
  ;
  ;; ;; way 2:
  ;;  (setq ac-clang-cflags (list
  ;;                        "-std=c++11"
  ;;                        "-stdlib=libc++"
  ;;                        "-frtti"
  ;;                        "-fexceptions"
  ;;                        "-DUSE_GLEW"
  ;;                        "-I/usr/local/include/c++/v1"
  ;;                        "-I/usr/local/include/c"
  ;;                        "-I/usr/local/include/c/sys"
  ;;                        ;; (concat "-I" (expand-file-name "~/Documents/fmesh_engine/engine/lib/package"))
  ;;                        "-I/usr/local/include"
  ;;                        "-I/usr/include"
  ;;                        ))
  )
 )

(require 'auto-complete-c-headers) ;; setup headers completion
(defun y:ac-clang-config ()
  ;; auto-complete (sources & headers) setting for C/C++ mode
  ;(setq ac-clang-async-do-autocompletion-automatically nil) ; disable auto-trigger
  ;; auto-complete C/C++ headers
  (cond
   ((string-equal system-type "gnu/linux")
    (setq ac-sources '(ac-source-clang-async
                       ac-source-c-headers
                       ;ac-source-semantic
                       ac-source-yasnippet))
                       ;ac-source-words-in-same-mode-buffers))
    (add-to-list 'achead:include-directories '"/usr/include")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/backward")
    (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/x86_64-unknown-linux-gnu")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include-fixed")
    (add-to-list 'achead:include-directories '"/usr/include/eigen3")
    (ac-clang-launch-completion-process))
   ((string-equal system-type "darwin")
    (cond
     ((string-equal y-clang-complete-type "clang-complete")
          ;;; use /emacs-clang-complete-clang/
      (setq ac-sources '(ac-source-clang
                         ac-source-c-headers
                         ;ac-source-semantic
                         ac-source-yasnippet))
                         ;ac-source-words-in-same-mode-buffers))
      )
     ((string-equal y-clang-complete-type "clang-complete-async")
          ;;; use /emacs-clang-complete-clang-async/
      (setq ac-sources '(ac-source-clang-async
                         ac-source-c-headers
                         ;ac-source-semantic
                         ac-source-yasnippet))
                         ;ac-source-words-in-same-mode-buffers))
      )
     )
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
)
(add-hook 'c-mode-hook 'y:ac-clang-config)
(add-hook 'c++-mode-hook 'y:ac-clang-config)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; setup compile command
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in gnome
(add-hook 'makefile-gmake-mode-hook
          (lambda () (define-key makefile-gmake-mode-map (kbd "C-c C-c") 'compile)))
;; default mode for Makefile in Mac OS X
(add-hook 'makefile-bsdmake-mode-hook
          (lambda () (define-key makefile-bsdmake-mode-map (kbd "C-c C-c") 'compile)))
;; ------------------------END----------------------------

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


;; -------------------------------------------
;; Enable major modes for CMake files
;; /cmake-mode/: cmake-mode.el
(require 'cmake-mode)
;; /cmake-font-lock/: to add more fontifying features
(add-to-list 'load-path "~/.emacs.d/git/cmake-font-lock")
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)


;; -------------------------------------------
;; ;; use /doxymacs/ to manipulate doxygen documentations
;; (add-to-list 'load-path "~/.emacs.d/git/doxymacs-1.8.0")
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; ; fontify the doxygen keywords
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)