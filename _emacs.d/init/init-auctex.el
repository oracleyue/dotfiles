;; ================================================================
;; Editing Environment for /LaTeX/
;; ================================================================

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
;; 8. "M-<tab>" complete macros
;; 9. "C-." flyspell auto correct words
;; 10."C-c _" to query for master files


(use-package tex
  :ensure auctex
  :demand
  :hook ((LaTeX-mode . LaTeX-math-mode)        ; math mode
         (LaTeX-mode . turn-on-reftex)         ; reftex
         ;; (LaTeX-mode . TeX-fold-mode)       ; source folding
         ;; (LaTeX-mode . outline-minor-mode)  ; outlining TeX
         )
  :config
  (setq TeX-auto-save           t
        TeX-parse-self          t
        reftex-plug-into-AUCTeX t)  ; when reftex is turned on

  ;; More extensions
  (setq auto-mode-alist
        (append '(("\\.tikz\\'" . latex-mode))
                auto-mode-alist))

  ;; Disable default TeX pairing
  (setq-default TeX-insert-braces nil)
  (setq-default LaTeX-electric-left-right-brace nil)
  (eval-after-load "latex" '(define-key LaTeX-mode-map "$" nil))
  ;; use /smartparens/ instead
  (with-eval-after-load "smartparens"
    (require 'smartparens-latex)
    (sp-with-modes '(latex-mode LaTeX-mode)
      (sp-local-pair "\\|" "\\|"
                     :trigger "\\|"
                     :unless '(sp-latex-point-after-backslash)
                     :when '(sp-in-math-p))
      (sp-local-pair "\\left|" "\\right|"
                     :trigger "\\l|"
                     :when '(sp-in-math-p))
      (sp-local-pair "\\big(" "\\big)"
                     :trigger "\\b("
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\big\\{" "\\big\\}"
                     :trigger "\\b{"
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\big[" "\\big]"
                     :trigger "\\b["
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "$" "$"
                     :actions '(insert wrap navigate))))

  ;; More math-mode in LaTeX
  (setq LaTeX-math-list
        '(("<"     "prec"         "Relational" nil)
          (">"     "succ"         "Relational" nil)
          ("v <"   "preceq"       "Relational" 10927)
          ("v >"   "succeq"       "Relational" 10928)
          ("="     "coloneqq"     "Relational" nil)
          ("v ="   "triangleq"    "Relational" nil)
          ("v ~"   "thicksim"     "Relational" nil)

          ("v ^"   "check"        "Accents" nil)

          ("o"     "otimes"       "Binary Op" nil)
          ("O"     "circ"         "Binary Op" nil)

          ("v 0"   "varnothing"   "Misc Symbol" nil)
          ("v n"   "nabla"        "Misc Symbol" nil)
          ("."     "dots"         "Misc Symbol" nil)
          ("v ."   "cdots"        "Misc Symbol" nil)
          ("T"     "top"          "Misc Symbol" nil)
          ("C-p"   "partial"      "Misc Symbol" nil)

          ("C-S-f" "longrightarrow" "Arrows" nil)
          ("C-S-b" "longleftarrow"  "Arrows" nil)
          ("C-m"   "longmapsto"     "Arrows" nil)

          ("C-t"   "textstyle"      "Misc" nil)     ;; overwirte \tan
          ("C-d"   "displaystyle"   "Misc" nil)     ;; overwirte \det
          ))

  ;; More math-font in LaTeX
  (setq LaTeX-font-list
        (quote ((1 "" "" "\\mathcal{" "}")
                (2 "\\textbf{" "}" "\\mathbf{" "}")
                (3 "\\textsc{" "}" "\\mathscr{" "}")
                (4 "" "" t)
                (5 "\\emph{" "}" "\\pmb{" "}")
                (6 "\\textsf{" "}" "\\mathsf{" "}")
                (9 "\\textit{" "}" "\\mathit{" "}")
                (13 "\\text{" "}")   ;; default: (13 "\\textmd{" "}")
                (14 "\\textnormal{" "}" "\\mathnormal{" "}")
                (18 "\\textrm{" "}" "\\mathrm{" "}")
                (19 "\\textsl{" "}" "\\mathbb{" "}")
                (20 "\\texttt{" "}" "\\mathtt{" "}")
                (21 "\\textup{" "}") )))

  ;; More keywords/macro fontify
  (setq font-latex-match-textual-keywords
        '(("smallskip" "")
          ("medskip" "")
          ("bigskip" "")
          ("noindent" "")
          ("indent" "")
          ("textstyle" "")
          ("displaystyle" "")
          ("protect")
          ("pause")
          ("makelecture" "")
          ("makeproblemset" "")
          ("solution" "")))
  (setq font-latex-match-variable-keywords
        '(("column" "{")
          ("url" "{")
          ("yue" "{")))
  (setq font-latex-match-reference-keywords
        '(("citep" "{")
          ("citet" "{")))

  ;; Extend reftex-citation
  ;; http://www.gnu.org/software/auctex/manual/reftex.html#SEC52
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands
  (eval-after-load 'reftex-vars
    '(progn
       (add-to-list 'TeX-style-path "~/.emacs.d/init/misc")))

  ;; Set master files for multiple documents
  ;; use "C-c _" to query for master files
  (setq-default TeX-master t)
  ;; (setq-default TeX-master 'dwim)

  ;; Bibliography for RefTeX
  (setq reftex-default-bibliography
        '("./library.bib" "./ref/library.bib" "../ref/library.bib"))
  ;; (setq reftex-bibliography-commands
  ;;       '("bibliography" "nobibliography" "addbibresource"))

  ;; Customize Compilation
  ;; modifying built-in commands
  (eval-after-load "tex"
    '(setcdr (assoc "LaTeX" TeX-command-list)
             '("%`%l%(mode) -shell-escape%' %t"
               TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))
  (eval-after-load 'latex
    '(setq LaTeX-clean-intermediate-suffixes
           (append LaTeX-clean-intermediate-suffixes (list "\\.spl" "\\.pyg" "\\.nlo" "\\.nls" "\\.fdb_latexmk" "\\.tdo"))))

  ;; indexing nomenclature and word index
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("Index (nomencl)" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                    TeX-run-TeX nil t :help "Run MakeIndex with nomencl")))

  ;; Xelatex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("XeLaTeX" "xelatex -synctex=1 -pdf -shell-escape %t" TeX-run-command nil t) t))

  ;; Rubber (python pkg)
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  ;; '("Rubber" "rubber --synctex -d %t" TeX-run-command nil t) t))
                  '("Rubber" "rubber --synctex --unsafe -fd %t" TeX-run-command nil t) t))
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("Rubber (xelatex)" "rubber --synctex --module xelatex %t" TeX-run-command nil t) t))
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("Clean (auto)" "rubber --clean %t; rm -rf auto/" TeX-run-command nil t) t))
  ;; if also wanting to delete pdf, use "rubber --pdf --clean %t".
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "Rubber")))

  ;; format conversion
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("pdf2png" "convert -density 300 %s.pdf -quality 90 %s.png" TeX-run-command nil t) t))

  ;; other bash assistance
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("update bib" "./supports/bibupdate.sh" TeX-run-command nil t) t))

  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("backup tex" "./supports/texbackup.sh %t" TeX-run-command nil t) t))

  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("backup tex (all)" "./supports/texbackup.sh" TeX-run-command nil t) t))

  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("update mathsym" "./supports/mathsym-update.sh" TeX-run-command nil t) t))

  ;; use latexmk
  (unless (file-exists-p (expand-file-name "~/.latexmkrc"))
    (copy-file (expand-file-name "~/.emacs.d/templates/_latexmkrc")
               (expand-file-name "~/.latexmkrc")))   ;; enable more functions
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("Latexmk" "latexmk -quiet -pdf %t" TeX-run-command nil t) t))

  ;; PDF Viewing
  (setq-default TeX-PDF-mode t)       ; default for pdf and forward search
  (setq TeX-source-correlate-mode t)  ; enable backward search PDF->LaTeX
  (cond
   ((string-equal system-type "gnu/linux")
    ;; Enable TeX <-> PDF sync
    (cond ((string= linux-desktop-env "gnome")
           ;; Use Evince
           (setq TeX-view-program-selection '((output-pdf "Evince"))))
          ((string= linux-desktop-env "kde")
           ;; Use Okular
           (setq TeX-view-program-selection '((output-pdf "Okular"))))
          ((string= linux-desktop-env "i3")
           ;; Use Zathura
           (setq TeX-view-program-selection '((output-pdf "Zathura"))))))
   ((string-equal system-type "darwin")
    ;; use skim as default pdf viewer
    ;; skim's displayline is used for forward search (from .tex to .pdf)
    ;; option -b highlights the current line; option -g opens Skim in the background
    (setq TeX-view-program-selection '((output-pdf "Skim")))
    (setq TeX-view-program-list
          '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))))
  ;; backward search: in skim's preference -> Sync -> PDF-TeX Sync support:
  ;; if using emacs server, set
  ;;  "Command": /usr/local/bin/emacsclient
  ;;  "Arguments": --socket-name=main --no-wait +%line "%file"

  ) ;; END of use-package(auctex)

;; Keybinding definitions
(eval-after-load "latex"
  '(progn
     ;; refresh and fontify buffer: =font-lock-fontify-buffer=
     ;; macro completions (flushed by flyspell.el)
     (define-key LaTeX-mode-map (kbd "M-<tab>") 'TeX-complete-symbol)))

;; Use /cdlatex/ to accelerate math typing
(use-package cdlatex
  :disabled
  :after tex
  :hook (LaTeX-mode . turn-on-cdlatex))

;; Utility definitions
(defun zyue/latex-remove-comments ()
  (interactive)
  (query-replace-regexp "\\(^\\| *[^\\\\]\\)%.*" "" nil nil))


(provide 'init-auctex)
;; ================================================
;; init-auctex.el ends here
