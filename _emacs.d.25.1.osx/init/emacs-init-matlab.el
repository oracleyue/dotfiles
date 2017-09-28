;; ================================================================
;; Programming Environment for /MATLAB/
;; ================================================================

(require 'matlab)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "/usr/local/bin/matlab")
(setq matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
(load-library "matlab-load")

;; enable CEDTE feature support
;(matlab-cedet-setup)

;; enable /company-dabbrev-code/ for /matlab-mode/
(add-to-list 'company-dabbrev-code-modes 'matlab-mode)

;; config /smartparens/ for matlab
(sp-with-modes 'matlab-mode
  (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                 :actions '(wrap autoskip navigate)))

;; Add "align" supports for matlab: align both "=" and "%"
;; alternatively, use "align-regexp" ("C-x M-a" defined in "emacs-init-basic.el"),
;; one may prefix "C-u" for more arguments.
(add-hook 'align-load-hook (lambda ()
   (add-to-list 'align-rules-list
                '(matlab-comment-align
                  (regexp . "\\(\\s-*\\)\\(%.*\\s-*\\)$")
                  (modes . '(matlab-mode))
                  (spacing . 4)
                  (repeat . nil)))
   (add-to-list 'align-rules-list
                '(matlab-equal-sign-align
                  (regexp . "\\(\\s-*\\)\\(=.*\\s-*\\)$")
                  (modes . '(matlab-mode))
                  (repeat . nil)))
   ))


(provide 'emacs-init-matlab)
;; ================================================
;; emacs-init-matlab.el ends here