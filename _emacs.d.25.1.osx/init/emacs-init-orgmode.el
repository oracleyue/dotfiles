;; ================================================================
;; Settings for /Org-mode/
;; ================================================================


;;
;; Basic Configuratoins
;;

(global-font-lock-mode 1)
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-cc" 'org-capture)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)

;; use variable fonts, like sans-serif
;; (add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))

;; set line space
;; (add-hook 'org-mode-hook (lambda () (setq line-spacing '0.25)))

;; startup styles
(setq org-startup-folded t)
;(setq org-startup-indented t)


;;
;; HTML Export Settings
;;
(setq org-export-html-style-extra "<style type=\"text/css\">\n  html {\n  font-family: sans-serif;\n  font-size: 10pt;\n  }\n  em { font-style: normal; font-weight: bold;}\n pre { \n  font-family: monospace;\n  font-size: 90%;\n } \n </style>")

;; Easy-Templates for LaTeX macros
(eval-after-load 'org
 '(progn
   (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n:AUTHOR:\n:CUSTOM_ID:\n:LABEL: sec:?\n:END:"))
   ;(add-to-list 'org-structure-template-alist '("eq" "\\begin{equation}\n?\n\\end{equation}"))
   ;(add-to-list 'org-structure-template-alist '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]"))
   ;(add-to-list 'org-structure-template-alist '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|"))
   ))


;;
;; Source Code Blocks and Babel
;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (C . t)
   (python . t)
   (R . t)
   (matlab . t)
   (latex . t)))
;(setq org-babel-python-command "python2")

;; stop asking evaluation codes when export
(setq org-export-babel-evaluate nil)

;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

;; no extra indentation
;(setq org-src-preserve-indentation t)

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))


;;
;; Auxiliary Configurations
;;

;; /smart-parens/ for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "$" "$"
                 :unless '(sp-latex-point-after-backslash-left))
  (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                 :actions '(insert wrap autoskip navigate escape))
  (sp-local-pair "\"" "\"" :unless '(sp-point-after-word-p)
                 :actions '(insert wrap autoskip navigate escape)))



(provide 'emacs-init-orgmode)
;; ================================================
;; emacs-init-orgmode.el ends here