; =======================================
;; For /AUCTeX-Mode/
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-PDF-mode t)    ; default for pdf and forward search
(setq TeX-source-correlate-mode t) ; enable backward search PDF->LaTeX
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)

;;;; More pair-mode in LaTeX
;;; Use /AucTeX/ default pairs
;(add-hook 'LaTeX-mode-hook
;	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;                      (cons "$" "$"))))
;(setq-default LaTeX-electric-left-right-brace t)
;;; Use /smartparens/ to complete pairs; having enable globally in /.emacs/
;; disable AucTeX pair completion
(setq-default LaTeX-electric-left-right-brace nil)
;; user-defined pairs
(defun sp-latex-point-after-backslash-left (id action context)
  "Return t if point follows a backslash, nil otherwise."
  (when (eq action 'insert)
    (let ((trigger (sp-get-pair id :trigger)))
      (looking-back (concat "\\\\l" (regexp-quote (if trigger trigger id)))))))
(sp-with-modes 'latex-mode
  (sp-local-pair "\\|" "\\|"
                 :trigger "\\|"
                 :unless '(sp-latex-point-after-backslash-left)
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left|" "\\right|"
                 :trigger "\\l|"
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left\\|" "\\right\\|"
                 :trigger "\\l\\|"
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  )

;; More math-mode in LaTeX
(setq LaTeX-math-list
      '(("<"   "preceq" "Relational" 10927)
        (">"   "succeq" "Relational" 10928)
        ("="   "triangleq"   "Relational" nil)
        ("v =" "coloneqq"    "Relational" nil)
        ("v ~" "thicksim"    "Relational" nil)
        ("v 0" "varnothing"    "Misc Symbol" nil)
        ("T"   "top"    "Misc Symbol" nil)
        ))
; More math-font in LaTeX
(setq LaTeX-font-list (quote ((1 "" "" "\\mathcal{" "}") (2 "\\textbf{" "}" "\\mathbf{" "}") (3 "\\textsc{" "}" "\\mathscr{" "}") (5 "\\emph{" "}") (6 "\\textsf{" "}" "\\mathsf{" "}") (9 "\\textit{" "}" "\\mathit{" "}") (13 "\\textmd{" "}") (14 "\\textnormal{" "}" "\\mathnormal{" "}") (18 "\\textrm{" "}" "\\mathrm{" "}") (19 "\\textsl{" "}" "\\mathbb{" "}") (20 "\\texttt{" "}" "\\mathtt{" "}") (21 "\\textup{" "}") (4 "" "" t))))

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("./ref/library.bib"))
; Adding -shell-escape in pdflatex for mint
(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))
(eval-after-load 'latex 
  '(setq LaTeX-clean-intermediate-suffixes
     (append LaTeX-clean-intermediate-suffixes (list "\\.spl" "\\.pyg"))))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("XeLaTeX" "xelatex -shell-escape %t" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("Rubber" "rubber -fd %t" TeX-run-command nil t) t))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "Rubber")))
;; (eval-after-load "tex"
;;    '(add-to-list 'TeX-command-list
;;                  '("Rubber-clean" "rubber --clean %t" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("convert to png" "convert -density 300 %s.pdf -quality 90 %s.png" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("update bib library" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/bibupdate.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup doc files" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/srcbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup tex files" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/texbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("Git regular push" "git add --all && git ci -m \"regular update\" && git push" TeX-run-command nil t) t))


(cond 
 ((string-equal system-type "gnu/linux")
  ; Use Evince as viewer, enable source <-> PDF sync
  (setq TeX-output-view-style
        (quote  (("^pdf$" "." "evince -f %o")
                 ("^html?$" "." "firefox %o"))))
  )
 ((string-equal system-type "darwin")
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
     ;; '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  )
)
