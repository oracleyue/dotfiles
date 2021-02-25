;; ================================================================
;; Settings for /Org-mode/
;; ================================================================
;; Last modified on 22 Oct 2020


;; /Basics/
(global-font-lock-mode t)

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; startup styles
(setq org-startup-folded   t
      org-startup-indented t)

;; view styles
(defun y/set-view-style-orgmode ()
  ;; line wraping
  (setq truncate-lines t)
  (turn-off-auto-fill)
  (if *use-sans-orgmode*
      (progn
        ;; use sans-serif
        (require 'org-variable-pitch)
        (org-variable-pitch-minor-mode t)
        ;; line spacing
        (setq line-spacing '0.25)
        ;; fill-column for sans
        (setq-local fill-column *fill-column-sans*))
    (setq-local fill-column *fill-column-mono*)))
(add-hook 'org-mode-hook #'y/set-view-style-orgmode)

;; show inline images
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)  ;; try using width specified by #+attr_*

;; highlight latex fragments
(setq org-highlight-latex-and-related '(latex script entities))

;; set apps to open files in orgmode
(setq org-file-apps (quote ((auto-mode       . emacs)
                            ("\\.x?html?\\'" . default)
                            ("\\.pdf\\'"     . default))))

;; diminish minor ("Ind" keyword in powerbar)
(eval-after-load "org-indent"
  '(diminish 'org-indent-mode))

;; use cdlatex for fast math typing
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;; /GTD Function Extensions/
;; refer to http://doc.norang.ca/org-mode.html

;; Todo keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))
(setq org-todo-keyword-faces '(("TODO"      . error)
                               ("NEXT"      . warning)
                               ("DONE"      . success)
                               ("WAITING"   . warning)
                               ("HOLD"      . default)
                               ("CANCELLED" . success)
                               ("MEETING"   . error))
      org-priority-faces '((?A . error)
                           (?B . warning)
                           (?C . success)))

;; Todo state triggers
(setq org-use-fast-todo-selection t)  ;; allow =C-c C-t= to enter KEY
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Agenda
(setq org-agenda-files (quote ("~/Public/Dropbox/oracleyue/OrgAgenda/ToDoList.org")))


;; /Export Settings/

;; HTML
(use-package htmlize)
;; local setting: add "#+HTML_HEAD" and "#+HTML_HEAD_EXTRA" in .org files
;; one can add "#+HTML_HEAD: " (leave empty) to disable global heads
;; (setq org-html-head-include-default-style nil)
(setq org-html-head
      (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
              (getenv "HOME")
              "/.emacs.d/templates/css/bootstrap.min.css\" />")
      org-html-head-extra
      (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
              (getenv "HOME")
              "/.emacs.d/templates/css/style.css\" />"))
;; use newer Mathjax
(require 'ox-html)
(setcdr (assoc 'path org-html-mathjax-options)
        '("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_HTML"))

;; Markdown (use ox-gfm)
;; (eval-after-load "org" '(require 'ox-md nil t))


;; /Code Blocks and Babel/

;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

;; use tab in .org to indent src blocks
(setq org-src-tab-acts-natively t)

;; setup babel for programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (C . t)
   (python . t)
   (R . t)
   (matlab . t)
   (ledger . t)
   (latex . t)))

;; stop asking evaluation codes when export
(setq org-export-babel-evaluate nil)

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))

;; structure templates
(require 'org-tempo)

;; NOT VALID after Emacs 27; to be fixed
;; (eval-after-load 'org
;;   '(progn
;;      (add-to-list 'org-structure-template-alist
;;        '("w" "#+BEGIN_WARNING\n?\n#+END_WARNING"))
;;      (add-to-list 'org-structure-template-alist
;;        '("tex" "#+BEGIN_LATEX\n?\n#+END_LATEX"))
;;      (add-to-list 'org-structure-template-alist
;;        '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]"))
;;      (add-to-list 'org-structure-template-alist
;;        '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|"))
;;      ))


;; ------------------------------------------------------------
;; External Minor Modes
;; ------------------------------------------------------------

;; /org-bullets/ to prettify UI
(use-package org-bullets
  :demand
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode)
  ;; :init (setq org-bullets-bullet-list '("●" "◉" "⚫" "•"))
  )

;; /smartparens/ for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "$" "$"
                 :unless '(sp-latex-point-after-backslash)
                 :actions '(insert wrap autoskip navigate escape))
  (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                 :actions '(insert wrap autoskip navigate escape)))

;; /org-download/ for image insertion
(use-package org-download
  :demand
  :bind (:map org-mode-map
              ("M-s s" . org-download-screenshot)
              ("M-s M-s" . org-download-screenshot))
  :config
  (setq-default org-download-image-dir "./img")
  (setq org-download-image-attr-list
        '("#+ATTR_HTML: :width 480px :align center"))
  (when *is-mac*
    ;; allow "ruby" in OSX -> Preferences -> Security & Privacy -> Screen Recording
    (setq org-download-screenshot-method "screencapture -i %s"))
  ;; show inline image in posframe ("C-c C-x C-v" to toggle)
  ;; (setq org-download-display-inline-images 'posframe)
  )

;; /ox-gfm/: github flavored markdown (gfm) exporter
;; note: it preserves soft line breaks.
(use-package ox-gfm
  :demand
  :config
  (eval-after-load "org" '(require 'ox-gfm nil t)))

;; /ox-reveal/: presentation via orgmode
(use-package ox-reveal
  :demand
  :config
  (if *use-css-local*
      (setq org-reveal-root (concat "file://" (getenv "HOME")
                                    "/Workspace/github/reveal.js/"))
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))
  (setq org-reveal-theme "black")  ;; klu
  (setq org-reveal-plugins '(highlight))
  (setq org-reveal-progress t)
  (setq org-reveal-title-slide
        "<h1>%t</h1><h3>%a</h3><h4>%e</h4><h4>%d</h4>"))



(provide 'init-orgmode)
;; ================================================
;; init-orgmode.el ends here
