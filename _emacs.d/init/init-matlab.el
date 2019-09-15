;; ================================================================
;; Programming Environment for /MATLAB/
;; ================================================================


;; Configurations
(use-package matlab-mode
  :config
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "/usr/local/bin/matlab")
  (setq matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
  (load "matlab-load" t t)

  ;; enable CEDTE feature support
  ;; (matlab-cedet-setup)

  ;; update/fix displays
  (defun zyue-matlab-customize-display ()
    (setq matlab-fill-column fill-column)
    (set-face-attribute 'font-lock-type-face nil :slant 'normal))
  (add-hook 'matlab-mode-hook 'zyue-matlab-customize-display)

  ;; enable /company-dabbrev-code/ for /matlab-mode/
  (require 'company-dabbrev-code)
  (add-to-list 'company-dabbrev-code-modes 'matlab-mode)

  ;; config /smartparens/ for matlab
  (sp-with-modes 'matlab-mode
    (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                   :actions '(insert wrap autoskip navigate)))

  ;; Add "align" supports for matlab: align both "=" and "%"
  ;; alternatively, use "align-regexp" ("C-x M-a" defined in "init-basic.el"),
  ;; one may prefix "C-u" for more arguments.
  (add-hook 'align-load-hook
            (lambda ()
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
                             (repeat . nil)))))
  )


(provide 'init-matlab)
;; ================================================
;; init-matlab.el ends here
