; =======================================
;; Programming Environment for /C C++/
(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ;; Having defined in /google-c-style/
;; (setq-default c-default-style "linux")
;; (setq-default c-basic-offset 4)
;; (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Fix /iedit/ bug in Mac; default key "C-c ;"
(require 'iedit)

;; /flymake-google-cpplint/ (having built-in /flymake-cursor/ functionality)
; let's define a function for flymake initialization
(defun y:flymake-google-init () 
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'y:flymake-google-init)
(add-hook 'c++-mode-hook 'y:flymake-google-init)

;; /google-c-style/
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; /xcscope/: source cross-referencing tool [install cscope]
;; (add-to-list 'load-path "~/.emacs.d/git/xcscope")
(require 'xcscope)
(cscope-setup)


;; configure /auto-complete/ for C/C++ sources and headers
;; -------------------BEGIN-------------------------------
(require 'auto-complete-c-headers)
;; /auto-complete-clang/: clang completion for C/C++ [For Mac OS X]
;; /auto-complete-clang-async/: clang completion for C/C++, compiling requested [For Linux]
;; setting for Linux & Mac OS X
(cond 
 ((string-equal system-type "gnu/linux")
  (add-to-list 'load-path "~/.emacs.d/git/clang-complete-async")
  (require 'auto-complete-clang-async)
  (setq ac-clang-complete-executable "~/.emacs.d/git/clang-complete-async/clang-complete") 
  (setq ac-clang-cflags    ;; for /emacs-clang-complete-async
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/x86_64-unknown-linux-gnu
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/backward
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include-fixed
 /usr/local/include
 /usr/include
                 ")))
  )
 ((string-equal system-type "darwin")
  ;; choose the clang-complete packages
  (setq y-clang-complete-type "clang-complete-async")
  ;; cflags/flags setting for ac-complete-clang
  (setq ac-clang-cflags       ;; for /emacs-clang-complete-async
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/local/include/eigen3
 /usr/local/include/c++
 /usr/local/include/c
 /usr/local/include
                 ")))
  (cond
   ((string-equal y-clang-complete-type "clang-complete-async")
        ;;; use /emacs-clang-complete-async/
    (add-to-list 'load-path "~/.emacs.d/git/clang-complete-async")
    (require 'auto-complete-clang-async)
    (setq ac-clang-complete-executable "~/.emacs.d/git/clang-complete-async/clang-complete-osx") 
    )
   ((string-equal y-clang-complete-type "clang-complete")
        ;;; use /emacs-clang-complete/
    (require 'auto-complete-clang)
    (setq ac-clang-flags ac-clang-cflags)    ;; for /emacs-clang-complete-async
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

(defun oy-ac-clang-config ()
  ;; auto-complete setting for C/C++ mode
  (setq ac-auto-start nil)   ; Note it is globally set to 4
  ;; auto-complete C/C++ headers
  (cond 
   ((string-equal system-type "gnu/linux")
    (setq ac-sources '(ac-source-clang-async 
                       ac-source-c-headers
                       ;ac-source-semantic
                       ac-source-words-in-same-mode-buffers))
    (add-to-list 'achead:include-directories '"/usr/include/c++/4.9.1")
    (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include")
    (ac-clang-launch-completion-process))
   ((string-equal system-type "darwin")
    (cond
     ((string-equal y-clang-complete-type "clang-complete")
          ;;; use /emacs-clang-complete-clang/
      (setq ac-sources '(ac-source-clang
                         ac-source-c-headers
                         ;ac-source-semantic
                         ac-source-words-in-same-mode-buffers))
      )
     ((string-equal y-clang-complete-type "clang-complete-async")
          ;;; use /emacs-clang-complete-clang-async/
      (setq ac-sources '(ac-source-clang-async 
                         ac-source-c-headers
                         ;ac-source-semantic
                         ac-source-words-in-same-mode-buffers))
      )
     )
    (add-to-list 'achead:include-directories '"/usr/local/include/eigen3")
    (add-to-list 'achead:include-directories '"/usr/local/include/c++")
    (add-to-list 'achead:include-directories '"/usr/local/include/c")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (ac-clang-launch-completion-process))
   )
)
(add-hook 'c-mode-hook 'oy-ac-clang-config)
(add-hook 'c++-mode-hook 'oy-ac-clang-config)
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

;; ;; use /doxymacs/ to manipulate doxygen documentations
;; (add-to-list 'load-path "~/.emacs.d/git/doxymacs-1.8.0")
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; ; fontify the doxygen keywords
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)