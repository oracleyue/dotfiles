;; ================================================================
;; Programming Environment for /MATLAB/
;; ================================================================
;; Last modified on 30 Aug 2020

(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :init
  ;; disable annoying prompt for adding "end" for functions
  (setq-default matlab-functions-have-end nil)
  :config
  (setq matlab-shell-command   "/usr/local/bin/matlab"
        matlab-shell-command-switches (quote ("-nodesktop -nosplash"))
        matlab-indent-function t
        matlab-fill-column     fill-column)

  ;; enable CEDTE feature support
  ;; (matlab-cedet-setup)

  ;; ----------- More supports for MATLAB -----------

  ;; enable /company-dabbrev-code/ for /matlab-mode/
  (require 'company-dabbrev-code)
  (add-to-list 'company-dabbrev-code-modes 'matlab-mode)

  ;; add /smartparens/ supports
  (with-eval-after-load "smartparens"
    (sp-with-modes 'matlab-mode
      (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                     :actions '(insert wrap autoskip navigate))))

  ;; use "align" for matlab: aligning both "=" and "%"
  ;; alternative: use "align-regexp" ("C-x M-a" in "init-edit.el"),
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
