;; ================================================================
;; Settings for /Org-mode/
;; ================================================================

;; Install required packages for more functions
(setq custom/org-packages
      '(htmlize))
(custom/install-packages custom/org-packages)


;;
;; Basic Configuratoins
;;

(global-font-lock-mode 1)
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-cc" 'org-capture)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)

;; startup styles
(setq org-startup-folded t)
;(setq org-startup-indented t)

;; use sans-serif for all texts
;; (add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))

;; set line space
;; (add-hook 'org-mode-hook (lambda () (setq line-spacing '0.25)))

;; set apps to open files in orgmode
(setq org-file-apps
      (quote ((auto-mode . emacs)
              ("\\.mm\\'" . default)
              ("\\.x?html?\\'" . default)
              ("\\.pdf\\'" . default))))


;;
;; Export Settings
;;

;; HTML
(setq org-html-head-include-default-style nil)
;; enable/disable on using global html head
(setq use-global-org-html-head t)
;; local  settings: add "#+HTML_HEAD" and "#+HTML_HEAD_EXTRA" in .org files
;; global settings: one may add "#+HTML_HEAD: " (leave empty) to disable global heads
(when use-global-org-html-head
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"https://cdn.rawgit.com/oracleyue/dotfiles/master/_emacs.d.25.1.osx/templates/css/bootstrap.min.css\" />")
  (setq org-html-head-extra
        "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"https://cdn.rawgit.com/oracleyue/dotfiles/master/_emacs.d.25.1.osx/templates/css/style.css\" />"))

;; Markdown
(eval-after-load "org" '(require 'ox-md nil t))


;;
;; Code Blocks and Babel
;;

;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

;; setup babel for programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; (sh . t)
   (C . t)
   (python . t)
   (R . t)
   (matlab . t)
   (latex . t)))

;; stop asking evaluation codes when export
(setq org-export-babel-evaluate nil)

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))


;;
;; Easy-Templates for LaTeX macros
;;
(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist
                  '("p" ":PROPERTIES:\n:AUTHOR:\n:CUSTOM_ID:\n:LABEL: sec:?\n:END:"))
     (add-to-list 'org-structure-template-alist
                  '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]"))
     (add-to-list 'org-structure-template-alist
                  '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|"))
     ))


;;
;; Additional minor modes supports
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