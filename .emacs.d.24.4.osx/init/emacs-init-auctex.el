; =======================================
;; Usages:
;; 1. if AuxTeX fails to fontify the buffer on time, run "M-x font-lock-fontify-buffer"
;; 2. "C-M-a" go to the beginning of the current environment;
;;    "C-M-e" go to the end of the current environment;
;; 3. "C-M-b/p" go to the beginning of the parenthesis
;;    "C-M-f/n" go to the end of the parenthesis
;; 4. "C-c ." mark the current environment, e.g. \section OR \begin ... \end
;; 5. kill the sentence :: "M-k"; go the beginning/end of the sentence :: "M-a/e"
;; 6. "C-c &" =reftex-view-crossref= display cross-ref info
;; 7. "C-q "" insert the double quote ", instead of ``''

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

;; More extensions in AUCTeX-mode
 (setq auto-mode-alist
     (append
         '(("\\.tikz\\'" . latex-mode))
          auto-mode-alist))

;; Basic settings
(setq TeX-insert-braces nil)

;; More pair-mode in LaTeX
;
;;; Use /AucTeX/ default pairs
;(add-hook 'LaTeX-mode-hook
;	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;                      (cons "$" "$"))))
;(setq-default LaTeX-electric-left-right-brace t)
;
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
                 :when '(sp-in-math-p))
                 ;; :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left|" "\\right|"
                 :trigger "\\l|"
                 :when '(sp-in-math-p))
                 ;; :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left\\|" "\\right\\|"
                 :trigger "\\l\\|"
                 :when '(sp-in-math-p))
                 ;; :post-handlers '(sp-latex-insert-spaces-inside-pair))
  )

;; More math-mode in LaTeX
(setq LaTeX-math-list
      '(
        ;; ("<"   "preceq" "Relational" 10927)
        ;; (">"   "succeq" "Relational" 10928)
        ("=" "coloneqq"    "Relational" nil)
        ("v ="   "triangleq"   "Relational" nil)
        ("v ~" "thicksim"    "Relational" nil)
        ("v 0" "varnothing"    "Misc Symbol" nil)
        ("."   "dots"    "Misc Symbol" nil)
        ("v ."   "cdots"    "Misc Symbol" nil)
        ("T"   "top"    "Misc Symbol" nil)
        ("C-p" "partial" "Misc Symbol" nil)
        ("C-c" "circ" "Misc Symbol" nil)  ;overwrite \cos
        ("C-t" "top" "Misc Symbol" nil)  ;overwirte \tan
        ("C-S-f" "longrightarrow" "Arrows" nil)
        ("C-S-b" "longleftarrow" "Arrows" nil)
        ("C-m" "longmapsto" "Arrows" nil)
        ))
; More math-font in LaTeX
(setq LaTeX-font-list (quote ((1 "" "" "\\mathcal{" "}") (2 "\\textbf{" "}" "\\mathbf{" "}") (3 "\\textsc{" "}" "\\mathscr{" "}") (5 "\\emph{" "}") (6 "\\textsf{" "}" "\\mathsf{" "}") (9 "\\textit{" "}" "\\mathit{" "}") (13 "\\textmd{" "}") (14 "\\textnormal{" "}" "\\mathnormal{" "}") (18 "\\textrm{" "}" "\\mathrm{" "}") (19 "\\textsl{" "}" "\\mathbb{" "}") (20 "\\texttt{" "}" "\\mathtt{" "}") (21 "\\textup{" "}") (4 "" "" t))))

;; More keywords/macro fontify
(setq font-latex-match-textual-keywords
      '(("smallskip" "")
        ("medskip" "")
        ("bigskip" "")
        ("noindent" "")
        ("indent" "")
        ("pause")
        ("makelecture" "")
        ("makeproblemset" "")
        ("solution" "")))
(setq font-latex-match-variable-keywords
      '(("column" "{")
        ("yue" "{")))

;; Extend reftex-citation
;http://www.gnu.org/software/auctex/manual/reftex.html#SEC52
;http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands
;http://tex.stackexchange.com/questions/220632/how-to-limit-auctex-search-for-style-subdirectories
;http://tex.stackexchange.com/questions/69031/auctex-style-for-siunitx
;; set directly (not nice)
;; (eval-after-load 'reftex-vars
;;   '(progn
;;      ;; (also some other reftex-related customizations)
;;      (setq reftex-cite-format
;;            '((?\r . "\\cite{%l}")
;;              (?f . "\\footcite{%l}")
;;              (?t . "\\textcite{%l}")))))
;; use style files
(eval-after-load 'reftex-vars
  '(progn 
     (add-to-list 'TeX-style-path "~/.emacs.d/init/styles")))

;; Default bibtex paths for RefTeX
(setq reftex-default-bibliography '("./ref/library.bib"))
;; More bibtex resouces
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
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
                 '("update bib library" "./bibupdate.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup doc files" "./srcbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup tex files" "./texbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("update userdef-mathsymb" "./mathsym_update.sh" TeX-run-command nil t) t))

;; (eval-after-load "tex"
;;    '(add-to-list 'TeX-command-list
;;                  '("Git regular push" "git add --all && git ci -m \"regular update\" && git push" TeX-run-command nil t) t))


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

;; define keybindings to refresh and fontify buffer
(eval-after-load "latex"
  '(progn
     (define-key LaTeX-mode-map (kbd "C-<f5>") 'font-lock-fontify-buffer)))