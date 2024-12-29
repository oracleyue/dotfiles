;; ================================================================
;; Settings for /Org-mode/
;; ================================================================
;; Last modified on 22 Oct 2020


;; /Basics/
(global-font-lock-mode t)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link) ;; "C-c C-l" to insert
(global-set-key (kbd "C-c b") 'org-switchb)

;; startup styles
(setq org-startup-folded     t
      org-startup-indented   t
      org-hide-leading-stars t)

;; view styles (line wraping, fill-column)
(defun y/set-view-style-orgmode ()
  (setq truncate-lines t)
  (turn-off-auto-fill)
  (if *use-sans-orgmode*
      (progn (require 'org-variable-pitch)
             (org-variable-pitch-minor-mode t)
             (setq line-spacing '0.25)
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
(setq gtd-home (expand-file-name
                "~/Public/Dropbox/oracleyue/GTD/"))
(setq note-home (expand-file-name
                 "~/Public/Dropbox/oracleyue/Notebooks/Research/"))
(setq todo-file    (expand-file-name "ToDoList.org" gtd-home))
(setq archive-file (expand-file-name "ArchivedDiary.org" gtd-home))
(setq temp-todo-file    (expand-file-name "inbox.org" gtd-home)) ;; used by iOS app
(setq idea-file    (expand-file-name "Ideas.org" note-home))
(setq seminar-file   (expand-file-name "Seminars.org" note-home))

(setq org-archive-location (concat archive-file "::")) ;; "C-c C-x C-a"

(setq org-agenda-files (list todo-file temp-todo-file))
(setq org-capture-bookmark nil)  ;; disable auto-add bookmark

;; Capture templates
(setq org-capture-templates
      '(("t" "TODO (ToDoList)" entry (file+headline todo-file "Collecting")
         "* TODO %? \nDEADLINE: %^t\nAdded on %U" :empty-lines 1)
        ("s" "Scheduled (ToDoList)" entry (file+headline todo-file "Collecting")
         "* NEXT %? %^G \nSCHEDULED: %^t\nAdded on %U" :empty-lines 1)
        ("n" "Quick notes (ToDoList)" entry (file+headline todo-file "Notes")
         "* %?\nAdded on %U\n" :empty-lines 1)
        ;; research notes
        ("i" "Ideas (Research)" entry (file idea-file)
         "* %?\nAdded on %U\n" :empty-lines 1)
        ("m" "Seminar notes (Research)" entry (file seminar-file)
         "* %?\nAdded on %U\n" :empty-lines 1)
        ))
;; (with-eval-after-load "counsel"
;;   (add-to-list 'ivy-initial-inputs-alist '(counsel-org-capture . "^")))

;; research diary: today/recent
(defun zyue/plan ()
  "Create a research diary for this month."
  (interactive)
  (progn (find-file todo-file)
         (goto-char (point-max))
         (insert "*" ?\s (format-time-string "%Y-%m %b") ?\n
                 "** Projects\n"
                 "** Research\n"
                 "** Review\n"
                 "** School\n"
                 "** Misc.\n"
                 "** Notes\n")))
;; [Note]: use default org-archive command "C-c C-x C-a"
;; archive of research diaries
;; (defun zyue/archive-plan ()
;;   "Archive the research diary of this month."
;;   (interactive)
;;   (progn (find-file archive-file)
;;          (goto-char (point-max))
;;          (insert-file todo-file)
;;          (save-buffer)
;;          (delete-file todo-file)  ;; clean up "ToDoList.org"
;;          (kill-buffer "ToDoList.org")))

;; Todo keywords
(defface org-doing
  '((t :foreground "white" :background "#75B5AA" :underline t))
  "Face for my own tag DOING."
  :group 'oracleyue)
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "DOING(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces '(("TODO"      . error)
                               ("WAITING"   . warning)
                               ("DONE"      . success)
                               ("NEXT"      . warning)
                               ("HOLD"      . default)
                               ("CANCELLED" . success)
                               ("DOING"     . org-doing))
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
   ;; (ledger . t)
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

;; /org-superstart/ for better UI
(use-package org-superstar
  :if (char-displayable-p ?⦿)
  :config
  (setq org-superstar-headline-bullets-list
        '("☰" "☷" "⦿" "✿" "✸" "●" "◆"))
  ;; avoid choosing unicode symbols intrinsically small
  ;; "☰" "☷" "☲" "☵" "⦿" "✿" "✸" "●" "⟐" "◆" "►"
  :hook (org-mode . org-superstar-mode))

;; /smartparens/ for org-mode
(with-eval-after-load "smartparens"
  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$"
                   :unless '(sp-latex-point-after-backslash)
                   :actions '(insert wrap autoskip navigate escape))
    (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                   :actions '(insert wrap autoskip navigate escape))))

;; /org-download/ for image insertion
(use-package org-download
  :demand
  :bind (:map org-mode-map
              ("M-s s" . org-download-screenshot)
              ("M-s D" . org-download-delete))  ;; "C-c C-o" to open attached file
  :config
  (setq-default org-download-image-dir "./img")
  (setq org-download-image-attr-list
        '("#+ATTR_HTML: :width 480px :align center"))
  (when *is-mac*
    ;; allow "ruby" in OSX: Preferences -> Security & Privacy -> Screen Recording
    (setq org-download-screenshot-method "screencapture -i %s"))
  ;; show inline image in posframe ("C-c C-x C-v" to toggle)
  ;; (setq org-download-display-inline-images 'posframe)
  )

;; /valign/: visual alignment for tables when using variable-pitch fonts
;; or Chinese
(use-package valign
  :demand
  :hook (org-mode . valign-mode))

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
  ;; use css locally or in github
  ;; (setq org-reveal-root (concat "file://" (getenv "HOME")
  ;;                                   "/Workspace/github/reveal.js/"))
  ;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-theme "black")  ;; klu
  (setq org-reveal-plugins '(highlight))
  (setq org-reveal-progress t)
  (setq org-reveal-title-slide
        "<h1>%t</h1><h3>%a</h3><h4>%e</h4><h4>%d</h4>"))

;; /ledger-mode/: financial accounting
;; provides Babel in org-mode for ledger src blocks.
(use-package ledger-mode
  :disabled
  :ensure nil)

;; /org-ref/: citation and cross-reference
(use-package org-ref
  :demand
  :after org
  :init
  ;; config bibtex-completion (the backend for "ivy-bibtex" in init-ivy.el)
  (setq bibtex-completion-bibliography
        '("~/Public/Dropbox/Academia/latex_templ/ref/library.bib")
        bibtex-completion-pdf-field "file"  ;; pdf file from bibtex entry
        bibtex-completion-notes-path "~/Public/Dropbox/oracleyue/Notebooks/Papers/"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
        ;; bibtex-completion-additional-search-fields '(keywords)

        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}"))
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎"
        bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "open" nil 0 nil fpath)))
  ;; config bibtex mode
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  :bind (:map org-mode-map
         ("C-c ]" . org-ref-insert-link)
         ("s-]"   . org-ref-insert-link-hydra/body)
         ;; "C-c C-o" on org-ref key triggers "org-ref-citation-hydra/body"
         :map bibtex-mode-map
         ("C-c C-o" . org-ref-bibtex-hydra/body)))


(provide 'init-orgmode)
;; ================================================
;; init-orgmode.el ends here
