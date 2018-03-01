;; ================================================================
;; Programming Environment for /Lisp/
;; ================================================================
;; use "C-j" in *scratch*; output is in the buffer under the sexp
;; use "C-h e" to open *Message* buffer for elisp printout
;; use "M-:" to evaluate elisp in mini-buffer
;; use "M-x ielm" to open interactive shell

;; To make "C-x C-e" use pretty-print; output is shown in minibuffer
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
;; Keybindings for eval expressions (default: "C-x C-e" eval last S-expr)
(define-key emacs-lisp-mode-map (kbd "C-c C-j") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)

;; /ParEdit/ mode
;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; MIT/GNU /Scheme/
(setq scheme-program-name "/usr/local/bin/mit-scheme")
(require 'xscheme)
;; Usage:
;;  - "M-x run-scheme" to invoke the Scheme process
;;  - "M-o" to send the buffer to the Scheme process



(provide 'init-lisp)
;; ================================================
;; init-lisp.el ends here
