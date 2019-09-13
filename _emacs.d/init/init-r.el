;; ================================================================
;; Programming Environment for /R/
;; ================================================================

;; Install required Emacs packages
;; (setq custom/r-packages
;;       '(ess
;;         flycheck
;;         key-combo))
;; (custom/install-packages custom/r-packages)
;; R packages: linter for R "lintr"


;; ESS configrations
(use-package ess
  :demand t
  :config
  ;; syntax checking (use flycheck)
  (setq ess-use-flymake nil) ;; diable bulit-in flymake

  ;; code completion (require R shell running)
  ;; - using built-in "ess-indent-or-complete" (C-M-i)
  ;; - using /company/: "company-R-args", "company-R-objects"
  ;;   and dabbev-code for variable names
  (setq ess-use-company t)
  (use-package company-dabbrev-code
    :ensure nil
    :after company
    :config
    (add-to-list 'company-dabbrev-code-modes 'ess-mode)
    (defun zyue/add-company-backend-ess ()
      (pop company-backends)
      (setq-local company-backends
                  (append '((company-R-args company-R-objects company-R-library
                                            company-dabbrev-code))
                          company-backends)))
    (add-hook 'ess-mode-hook 'zyue/add-company-backend-ess))
  )

;; Quick operators by /key-combo/
(use-package key-combo
  :init
  (add-hook 'ess-mode-hook '(lambda() (key-combo-mode t)))
  (add-hook 'inferior-ess-mode-hook '(lambda() (key-combo-mode t)))

  (defvar key-combo-ess-default
    '((">"  . (" > " " %>% " " %>>% "))
      ("$"  . ("$" " %$% "))
      ("<>" . " %<>% ")
      ("%"  . ("%" "%%"))  ; not working!
      ("^"  . ("^" " ^ "))
      ("!"  . ("!" " != "))
      (":"  . (":" "::" ":::"))
      (":=" . " := ") ; data.table
      ("->" . " -> ")
      ("<-" . " <- ")
      (","  . (", " ","))
      ("~"  . (" ~ " "~"))
      ("="  . (" = " " == " "="))
      ("*"  . (" * " " %*% " " ** "))
      ("/"  . (" / " "/"))
      ("+"  . (" + " "+"))
      ("-"  . (" - " "-"))
      ("|"  . (" | " "|"))
      ))
  (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
                         'ess-key-combo-load-default key-combo-ess-default))


(provide 'init-r)
;; ================================================
;; init-r.el ends here
