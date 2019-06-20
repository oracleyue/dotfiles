;; ================================================================
;; Programming Environment for /R/
;; ================================================================

;; Install required Emacs packages
(setq custom/r-packages
      '(ess
        flycheck
        key-combo))
(custom/install-packages custom/r-packages)
;; R packages: linter for R "lintr"


;; Configrations
(use-package ess-site
  :demand t
  :config
  ;; syntax checking (use flycheck)
  (setq ess-use-flymake nil) ;; diable bulit-in flymake

  ;; code completion (require R shell running)
  ;; - using built-in "ess-indent-or-complete" (C-M-i)
  ;; - using /company/: "company-R-args", "company-R-objects"
  ;;   and dabbev-code for variable names
  (use-package company-dabbrev-code
    :after company
    :config
    (add-to-list 'company-dabbrev-code-modes 'ess-mode)
    (defun zyue/add-company-backend-ess ()
      (pop company-backends)
      (setq-local company-backends
                  (append '((company-R-args company-R-objects company-dabbrev-code))
                          company-backends)))
    (add-hook 'ess-mode-hook 'zyue/add-company-backend-ess))

  ;; adding operator support in ESS via /key-combo/
  (use-package key-combo
    :config
    (key-combo-mode 1)

    (add-hook 'ess-mode-hook '(lambda() (key-combo-mode t)))
    (add-hook 'inferior-ess-mode-hook '(lambda() (key-combo-mode t)))

    (defvar key-combo-ess-default
      '((">"  . (" > " " %>% "))
        ("_"  . ("_" " <- "))
        ("="  . (" = " " == "))
        ("$"  . ("$" " %$% "))
        ("<>" . " %<>% ")
        ("*"  . ("*" " * "))
        ("%" . ("%" "%*%" "%%"))
        ("^"  . ("^" " ^ "))
        ("/"  . ("/" " / "))
        ("~" . " ~ ")
        (":" . (":" "::" ":::"))
        (":="  . " := ") ; data.table
        ("->"  . " -> ")
        ("<-"  . " <- ")
        ))

    (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
                           'ess-key-combo-load-default key-combo-ess-default))

  ) ;; END of ess-site



(provide 'init-r)
;; ================================================
;; init-r.el ends here
