;; ===============================================================
;; Settings for /Org-mode/
;; ===============================================================

;; Required software in the system:
;; - "ledger": financial accounting


;;
;; Configuratoins of Orgmode
;;

(global-font-lock-mode 1)
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; startup styles
(setq org-startup-folded t)
(setq org-startup-indented nil)

;; view styles
(defun y/set-view-style-orgmode ()
  (when *is-server-main*
    (variable-pitch-mode t)    ;; use sans-serif
    (setq line-spacing '0.25)) ;; line spacing
  (setq truncate-lines t)      ;; line wraping
  (turn-off-auto-fill)
  (setq-local fill-column 80))
(add-hook 'org-mode-hook #'y/set-view-style-orgmode)

;; highlight latex fragments
(setq org-highlight-latex-and-related '(latex script entities))

;; set apps to open files in orgmode
(setq org-file-apps
      (quote ((auto-mode . emacs)
              ("\\.mm\\'" . default)
              ("\\.x?html?\\'" . default)
              ("\\.pdf\\'" . default))))


;; Export Settings

;; HTML
(use-package htmlize :ensure t)  ;; code block fontification
;; using css styles
(setq org-html-head-include-default-style nil)
;; local setting: add "#+HTML_HEAD" and "#+HTML_HEAD_EXTRA" in .org files
;; one can add "#+HTML_HEAD: " (leave empty) to disable global heads
(if *use-css-local*
    (progn
      (setq org-html-head
            "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"/Users/oracleyue/.emacs.d/templates/css/bootstrap.min.css\" />")
      (setq org-html-head-extra
            "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"/Users/oracleyue/.emacs.d/templates/css/style.css\" />"))
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"https://rawgit.com/oracleyue/dotfiles/master/_emacs.d/templates/css/bootstrap.min.css\" />")
  (setq org-html-head-extra
        "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"https://rawgit.com/oracleyue/dotfiles/master/_emacs.d/templates/css/style.css\" />"))

;; Markdown
(eval-after-load "org" '(require 'ox-md nil t))


;; Code Blocks and Babel

;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

;; setup babel for programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell      . t)
   (C          . t)
   (python     . t)
   (R          . t)
   (matlab     . t)
   (ledger     . t)  ;; use ~ledger~ for finance accounting
   (latex      . t)))

;; stop asking evaluation codes when export
(setq org-export-babel-evaluate nil)

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))


;; Easy-Templates for LaTeX macros
(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist
       '("w" "#+BEGIN_WARNING\n?\n#+END_WARNING"))
     (add-to-list 'org-structure-template-alist
       '("p" ":PROPERTIES:\n:AUTHOR:\n:END:"))
     (add-to-list 'org-structure-template-alist
       '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]"))
     (add-to-list 'org-structure-template-alist
       '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|"))
     ))


;;
;; Additional Supports via Third-party Packages
;;

;; additional config of /smartparens/ for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "$" "$"
                 :unless '(sp-latex-point-after-backslash)
                 :actions '(insert wrap autoskip navigate escape))
  (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                 :actions '(insert wrap autoskip navigate escape)))


;; /ox-gfm/: github flavored markdown (gfm) exporter
(use-package ox-gfm
  :ensure t
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))


;; ;; /ox-reveal/: presentation via orgmode
;; (use-package ox-reveal
;;   :commands (org-reveal)
;;   :init
;;   (add-hook 'after-init-hook #'org-reveal)
;;   :config
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   (setq org-reveal-theme "black")
;;   (setq org-reveal-width 1200)
;;   (setq org-reveal-height 1000)
;;   (setq org-reveal-margin "0.1")
;;   (setq org-reveal-min-scale "0.5")
;;   (setq org-reveal-max-scale "2.5")
;;   (setq org-reveal-transition "cube")
;;   (setq org-reveal-plugins '(classList markdown zoom notes))
;;   (setq org-reveal-control t)
;;   (setq org-reveal-center t)
;;   (setq org-reveal-progress t)
;;   (setq org-reveal-history nil))


;; /ledger-mode/: financial accounting
(use-package ledger-mode
  :ensure t)



(provide 'init-orgmode)
;; ================================================
;; init-orgmode.el ends here
