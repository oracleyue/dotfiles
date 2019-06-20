;; ================================================================
;; Settings for /Org-mode/
;; ================================================================

;; Install required packages for more functions
(setq custom/org-packages
      '(htmlize
        smartparens
        ox-gfm
        ox-reveal))
(custom/install-packages custom/org-packages)


;;
;; Configuratoins of Org-Mode
;;

;; /Basic/

(global-font-lock-mode 1)
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; startup styles
(setq org-startup-folded t)
(setq org-startup-indented t)

;; view styles
(defun y/set-view-style-orgmode ()
  (when *is-server-main*
    ;; (variable-pitch-mode t)    ;; use sans-serif
    (setq line-spacing '0.25)) ;; line spacing
  (setq truncate-lines t)      ;; line wraping
  (turn-off-auto-fill)
  (setq-local fill-column *fill-column-sans*))
(add-hook 'org-mode-hook #'y/set-view-style-orgmode)

;; highlight latex fragments
(setq org-highlight-latex-and-related '(latex script entities))

;; set apps to open files in orgmode
(setq org-file-apps
      (quote ((auto-mode . emacs)
              ("\\.mm\\'" . default)
              ("\\.x?html?\\'" . default)
              ("\\.pdf\\'" . default))))

;; /Export Settings/

;; HTML
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
;; use newer Mathjax
(require 'ox-html)
(setcdr (assoc 'path org-html-mathjax-options)
        '("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_HTML"))

;; Markdown
(eval-after-load "org" '(require 'ox-md nil t))

;; /Code Blocks and Babel/

;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

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

;; /Easy-Templates for LaTeX macros/

(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist
       '("w" "#+BEGIN_WARNING\n?\n#+END_WARNING"))
     (add-to-list 'org-structure-template-alist
       '("tex" "#+BEGIN_LATEX\n?\n#+END_LATEX"))
     (add-to-list 'org-structure-template-alist
       '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]"))
     (add-to-list 'org-structure-template-alist
       '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|"))
     ))


;;
;; Additional Supports via Third-party Packages
;;

;; /smartparens/ for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "$" "$"
                 :unless '(sp-latex-point-after-backslash)
                 :actions '(insert wrap autoskip navigate escape))
  (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                 :actions '(insert wrap autoskip navigate escape)))

;; /ox-gfm/: github flavored markdown (gfm) exporter
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; /ox-reveal/: presentation via orgmode
(use-package ox-reveal
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


;;
;; User-defined utility enhancement
;;

(defun zyue/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
					; take screenshot
  (if (eq system-type 'darwin)
      (progn
	(call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
									    "\"" filename "\"" ))
	(call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
	))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
					; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-display-inline-images))
(global-set-key (kbd "C-c s c") 'zyue/org-screenshot)



(provide 'init-orgmode)
;; ================================================
;; init-orgmode.el ends here
