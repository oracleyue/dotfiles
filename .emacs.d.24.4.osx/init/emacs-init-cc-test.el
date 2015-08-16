;; ==========================================
;; Additional configurations for C/C++ environment
;; (must be loaded after /helm/ due to semantic loading for helm-semantic-or-imenu)

;; Package: /yasnippet/
(require 'popup)
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)
(setq-default mode-require-final-newline nil)
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-x-prompt yas-no-prompt))   ; note that yas-dropdown-prompt needs /dropdown-


;; Package: /semantic-mode/ in /CEDET/
;; basic config
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
;; setting include paths
(semantic-add-system-include "/usr/local/include/boost" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c++-mode)


;; Package: /function-args/
;; - keybinding: fa-show =M-i=; moo-complete =M-o=
(require 'function-args)
(add-hook 'c-mode-hook 'fa-config-default)
(add-hook 'c++-mode-hook 'fa-config-default)
;; enable case-insensitive searching
(set-default 'semantic-case-fold t)
;; put c++-mode as default for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; improve the parse of macro-heavy code 
(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
    "
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/usr/include/stddef.h
")


;; Package: /company-mode/
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;(add-hook 'c-mode-common-hook 'company-mode)
;; (add-hook 'c-mode-common-hook (lambda ()
;;                                 (setq ac-auto-start nil)))
;; configurations
(setq company-idle-delay 0)
(face-remap-add-relative 'company-tooltip-annotation :foreground "white")
;; source code completion using clang
;(setq company-backends (delete 'company-semantic company-backends))
;; keybindings
(define-key c-mode-map [(control tab)] 'company-complete)
(define-key c++-mode-map [(control tab)] 'company-complete)
;; Package: /company-c-headers/
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/local/include/c++")
(add-to-list 'company-c-headers-path-system "/usr/local/include/boost")


