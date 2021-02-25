;; ================================================================
;; Programming Environment for /MATLAB/
;; ================================================================
;; Last modified on 30 Aug 2020


(use-package matlab-mode
  :config
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
  (setq matlab-shell-command "/usr/local/bin/matlab"
        matlab-shell-command-switches (quote ("-nodesktop -nosplash"))
        matlab-indent-function t
        matlab-fill-column fill-column)
  (load "matlab-load" t t)

  ;; enable CEDTE feature support
  ;; (matlab-cedet-setup)

  ;; disable prompt for "end" completion
  (setq-default matlab-functions-have-end nil)

  ;; enable /company-dabbrev-code/ for /matlab-mode/
  (require 'company-dabbrev-code)
  (add-to-list 'company-dabbrev-code-modes 'matlab-mode)

  ;; "matlab function..end mode" in matlab-mode
  (eval-after-load "matlab"
    '(diminish 'matlab-functions-have-end-minor-mode))


  ;; ================================================================
  ;; Support Extensions for MATLAB
  ;; ================================================================

  ;; Add /smartparens/ supports
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

  ) ;; END of use-package


(provide 'init-matlab)
;; ================================================
;; init-matlab.el ends here
