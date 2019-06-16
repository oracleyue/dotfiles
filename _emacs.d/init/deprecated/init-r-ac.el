;; Programming Environment for /R/
(require 'ess-site)

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

;; fix bugs in ac due to line wrap (fixed, not needed)
;; (add-hook 'ess-mode-hook (lambda () (setq truncate-lines t)))

;; disable understore behaviors
(ess-toggle-underscore nil)

;; enable ess-indent-or-complete for completion
;; (setq ess-tab-complete-in-script t)

;; adding operator support in ESS via /key-combo/
(require 'key-combo)
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
    ("->"  . " -> ")))

(key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
                       'ess-key-combo-load-default key-combo-ess-default)

;; code complete: use /auto-complete/
;; Notes:
;;   support automatically by ESS-mode; however, the R shell has to be run
;;   and the codes must be no synatax errors.
;;   use "ac-source-R-args", "ac-source-R-objects", "ac-source-R" as backends


(provide 'init-r-ac)
;; ================================================
;; init-r-ac.el ends here
