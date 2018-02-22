(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; Your init file should contain only one such instance.
 '(fill-column 80))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; Your init file should contain only one such instance.
 '(default ((t (:font "DejaVu Sans Mono-15"))))  ;; 13/15(mac), 10.5/12(linux)
 '(fixed-pitch ((t (:family "Roboto Mono"))))
 '(variable-pitch ((t (:family "Roboto")))))

;; stop emacs automatically editing .emacs
(setq disabled-command-function nil)
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; load paths
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "git" user-emacs-directory))

;; packages
;; (package-initialize)
(require 'emacs-init-packages)

;; theme
(require 'emacs-init-theme)

;; basics
(require 'emacs-init-basics)
(require 'emacs-init-edit)
;; (require 'emacs-init-evil)           ;; use vim in emacs

;; feature control
(if (or (string-equal "main" (daemonp)) (not (daemonp)))
    (setq y:enable-cedet-semantics nil) ;; /helm-sematic-or-imenu/, /stickyfunc/
  (setq y:enable-cedet-semantics t))
(setq y:enable-function-args nil)       ;; /function-args/ (require semantics)
(setq y:use-direx-or-neotree            ;; enable /direx-jedi/ or/and /neotree/
      '(("direx-jedi" . nil) ("neotree" . t)))
(setq y:enable-google-cpp-style nil)    ;; /google-c-style/
(setq y:cc-complete-engine "irony")     ;; company-clang, irony, modern

;; select completion engine for servers
(cond ((string-equal system-type "darwin")
       (if (string-equal "ac-mode" (daemonp))
           (setq y:code-complete-mode "auto-complete")
         (setq y:code-complete-mode "company")))
      ((string-equal system-type "gnu/linux")
       (setq y:code-complete-mode "company")))

;; code completion engine
(if (string-equal y:code-complete-mode "company")
    (require 'emacs-init-company)       ;; use /company/
  (require 'emacs-init-ac))             ;; use /auto-complete/

;; helm and extensions
(require 'emacs-init-helm)
(require 'emacs-init-helm-swoop)
(require 'emacs-init-helm-proj)         ;; project management

;; directory and buffer explorers
(require 'emacs-init-dired)             ;; directory explorers

;; major modes for document editing
(require 'emacs-init-orgmode)           ;; org-mode
(require 'emacs-init-markdown)          ;; markdown
(require 'emacs-init-auctex)            ;; latex


;; ----------------------------------------------------------------
;; Programming Environments
;; ----------------------------------------------------------------

;; general programming supports
(require 'emacs-init-progsupp)          ;; edit, vc, debug, ui ...
(require 'emacs-init-gtags)             ;; code navigation

;; programming environment for /Lisp/
(require 'emacs-init-lisp)

;; programming environment for /HTML, js/
(require 'emacs-init-web)

;; programming environment for /C C++/
(if (string-equal y:code-complete-mode "company")
    (cond ((string-equal y:cc-complete-engine "clang")       ;; company-clang
           (require 'emacs-init-cc-comp))
          ((string-equal y:cc-complete-engine "irony")       ;; irony
           (org-babel-load-file
            (expand-file-name "init/emacs-init-cc-irony.org"
                              user-emacs-directory)))
          ((string-equal y:cc-complete-engine "modern")      ;; rtags + irony
           (org-babel-load-file
            (expand-file-name "init/emacs-init-cc-modern.org"
                              user-emacs-directory))))
  (require 'emacs-init-cc-ac))          ;; use /auto-complete/

;; programming environment for /Python/
(if (string-equal y:code-complete-mode "company")
    (require 'emacs-init-py-comp)       ;; use /company/
  (require 'emacs-init-py-ac))          ;; use /auto-complete/

;; programming environment for /R/
(if (string-equal y:code-complete-mode "company")
    (require 'emacs-init-r-comp)        ;; use /company/
  (require 'emacs-init-r-ac))           ;; use /auto-complete/

;; programming environment for /MATLAB/
(require 'emacs-init-matlab)

;; major-modes for minority languages
(require 'emacs-init-proglang)

;; ----------------------------------------------------------------


;; Restore default configurations overwritten by other modes
(require 'emacs-init-restore)
