;; ==========================================
;; Settings for /Org-mode/

;; Activation
;(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-font-lock-mode 1)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Font faces
;; use variable fonts, like sans-serif
;; (add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
;; force to use fixed-width fonts
;(add-hook 'org-mode-hook
;          (lambda () (face-remap-add-relative 'default :family "DejaVu Sans Mono")))
;; set line space
(add-hook 'org-mode-hook (lambda () (setq line-spacing '0.25)))

(setq org-startup-folded t)
;(setq org-startup-indented t)
;(add-hook 'org-mode-hook (lambda()  (visual-line-mode t)))

;; HTML Export Settings
(setq org-export-html-style-extra "<style type=\"text/css\">\n  html {\n  font-family: sans-serif;\n  font-size: 10pt;\n  }\n  em { font-style: normal; font-weight: bold;}\n pre { \n  font-family: monospace;\n  font-size: 90%;\n } \n </style>") 

;; Easy-Templates for LaTeX macros
(eval-after-load 'org
 '(progn
   ;(add-to-list 'org-structure-template-alist '("eq" "\\begin{equation}\n?\n\\end{equation}")) 
   ;(add-to-list 'org-structure-template-alist '("eqa" "\\begin{equation}\n \\begin{array}{}\n?\n \\end{array}\n\\end{equation}")) 
   ;(add-to-list 'org-structure-template-alist '("bm" "\\begin{bmatrix}\n?\n\\end{bmatrix}")) 
   (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n:AUTHOR:\n:CUSTOM_ID:\n:LABEL: sec:?\n:END:")) 
   ;(add-to-list 'org-structure-template-alist '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]")) 
   ;(add-to-list 'org-structure-template-alist '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|")) 
   (add-to-list 'org-structure-template-alist '("uml" "#+BEGIN_SRC plantuml :file files/dia#.png :exports results\n?\n#+END_SRC")) 
))
;; Setup of Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (latex . t)
   (ditaa . t)
   (plantuml . t)
  )) 
(setq org-babel-python-command "python2")
(setq org-export-babel-evaluate nil)
(setq org-plantuml-jar-path
  (expand-file-name "/usr/share/emacs/24.3/lisp/contrib/scripts/plantuml.jar"))

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)
; fix bug to load org-block settings in themes by re-enabling it
(defface org-block-begin-line
  '((t (:slant italic)))
   "Face used for the line delimiting the begin of source blocks.")
(defface org-block-end-line
  '((t (:slant italic)))
  "Face used for the line delimiting the end of source blocks.")
;; no extra indentation
(setq org-src-preserve-indentation t)
;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))

;; Setting for /org-article/ for LaTeX
(require 'ox-latex)
(setq org-export-latex-listings 'minted)
(setq org-export-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

(setq org-export-latex-packages-alist 
   '(("AUTO" "inputenc" t)
; font type settings:
    ("" "mathptmx" t)
    ("scaled=0.8" "DejaVuSansMono" t)
; math symbols and figures:
    ("" "latexsym" t)
    ("" "amssymb" t)
    ("" "amsmath" t) 
    ("" "amsthm" t)
    ("" "graphicx" t)
    ("" "subfigure" t)
    ("" "epsfig" t)
; others    
    ("usenames" "color" t)
    ("" "csquotes" t)
	("" "minted" t) ; nil by default
    ("" "hyperref" t)
))

;; do not put in \hypersetup use your own
;; \hypersetup{pdfkeywords={%s},\n pdfsubject={%s},\n pdfcreator={%s}
(setq org-latex-with-hyperref nil)

(add-to-list 'org-latex-classes '("org-article" 
"\\documentclass{org-article}
\\usepackage[top=1in, bottom=1in, left=1.2in, right=1.2in]{geometry}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
)

;;; Seem not required in new version
;; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "bibtex %b"
;;         "makeindex %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))


;;; /smart-parens/ for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "$" "$"
                 :trigger "$"
                 :unless '(sp-latex-point-after-backslash-left)))
