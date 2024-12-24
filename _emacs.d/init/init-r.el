;; ================================================================
;; Programming Environment for /R/
;; ================================================================

;; Install R packages: linter for R "lintr"


;; ESS mode
(use-package ess
  :init (require 'ess-site)
  :config
  ;; syntax checking (use flycheck instead)
  (setq ess-use-flymake nil) ;; diable bulit-in flymake

  ;; built-in code completion (require R shell running)
  ;; - using built-in "ess-indent-or-complete" (C-M-i)
  ;; - using /company/: check "init-company.el"
  ;;   (add "company-R-args", "company-R-objects" and dabbev-code to backends)
  ;; (setq ess-use-company t)

  ;; LSP for code completion (check init-lsp-bridge.el)
  )

;; /key-combo/: quick operators
(use-package key-combo
  :init
  (add-hook 'ess-mode-hook #'(lambda() (key-combo-mode t)))
  (add-hook 'inferior-ess-mode-hook #'(lambda() (key-combo-mode t)))

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
