;; Programming Environment for /R/
(use-package ess-site
  :config

  ;; set additional font lock
  (setq ess-R-font-lock-keywords
        (quote
         ((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T)
          (ess-R-fl-keyword:%op%))))

  ;; disable understore behaviors
  (ess-toggle-underscore nil)

  ;; code completion:
  ;; use built-in: ess-indent-or-complete (use "TAB", mini-buffer)
  ;; (setq ess-tab-complete-in-script t)
  ;; use company-mode
  (if *integrate-TAB*
      (if (display-graphic-p)
          (define-key ess-mode-map (kbd "<tab>") 'tab-indent-or-complete)
        (define-key ess-mode-map (kbd "TAB") 'tab-indent-or-complete)))

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

  ;; code complete: use /company/
  ;; Notes:
  ;;   support automatically by ESS-mode; however, the R shell has to be run
  ;;   and the codes must be no synatax errors.
  ;;   use "company-R-args", "company-R-objects" as backends

  ;; add dabbev-code in backends to complete variable names
  (require 'company-dabbrev-code)
  (add-to-list 'company-dabbrev-code-modes 'ess-mode)
  (defun y:add-company-backend-ess ()
    (pop company-backends)
    (setq-local company-backends
                (append '((company-R-args company-R-objects company-dabbrev-code))
                        company-backends)))
  (add-hook 'ess-mode-hook 'y:add-company-backend-ess)

  ;; delete the unnecessary buffer *ESS*
  (kill-buffer "*ESS*")

  ) ;; END of ess-site



(provide 'init-r)
;; ================================================
;; init-r.el ends here
