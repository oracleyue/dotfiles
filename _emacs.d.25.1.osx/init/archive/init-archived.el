;; =====================================
;; This file saves all configurations of modes/packages that no longer used.
;; --- oracleyue

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; For /folding-mode/
;(add-to-list 'load-path "~/.emacs.d/git")
;(require 'folding)
;(load "folding" 'nomessage 'noerror)
;(folding-mode-add-find-file-hook)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /Icomplete Mode/
;; (icomplete-mode 1)    ; Turn on icomplete-mode

;; ;; /ido, ido-ubiquitous, flx-ido/
;; (require 'ido)
;; (require 'ido-ubiquitous)
;; (require 'flx-ido)

;; ;; Baisc configuration
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-max-prospects 10
;;       ido-save-directory-list-file "~/.emacs.d/userdata/ido.hist"
;;       ido-default-file-method 'selected-window
;;       ido-auto-merge-work-directories-length -1)

;; ;; Enable modes
;; (ido-mode t)
;; (ido-ubiquitous-mode t)
;; (flx-ido-mode t)          ;smarter fuzzy matching for ido

;; ;; Keybindings
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;; ;; Extensions ordering and ignored ones
;; (setq ido-file-extensions-order '(".tex" ".org" ".py" ".cpp" ".hpp" ".sh" ".xml" ".el"))
;; (setq completion-ignored-extensions '(".o" ".elc" "~" ".obj" ".a" ".so" ".aux" ".out" ".pyg" "blg" "log" ".synctex.gz" "toc" "bbl"))
;; (setq ido-ignore-extensions t)

;; ;; Keep annoying buffers out of search
;; (setq ido-ignore-buffers (list (rx (or (and bos  " ")
;;                                        (and bos
;;                                             (or "*Completions*"
;;                                                 "*Shell Command Output*"
;;                                                 "*vc-diff*")
;;                                             eos)))))
;; ;; Show ido results vertically, rather than horizontally
;; ;(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
;; ;  [No match]" " [Matched]" " [Not readable]" " [Too big]" "
;; ;  [Confirm]")))
;; ;(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; ;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; ;; /smex/ remember recently and most frequently used commands
;; (require 'smex)
;; (setq smex-save-file "~/.emacs.d/userdata/.smex-items")
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ;;/ido-hacks/
;; (require 'ido-hacks nil t)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /Multi-Term/
;; (require 'multi-term)
;; (setq multi-term-program "/bin/bash")
;; (add-hook 'term-mode-hook 'evil-emacs-state)
;; (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
;; ;; key bindings for delicated term
;; (add-to-list 'term-bind-key-alist '("C-=" . multi-term-next))
;; (global-set-key (kbd "C-x t") 'multi-term-dedicated-toggle)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /ERC/
;; (setq erc-default-coding-system '(utf-8 . utf-8))
;; (setq erc-nick "oracleyue"
;;      erc-user-full-name "oracleyue")

;; ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /w3m/ web browser
;; ;(setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;     ;; optional keyboard short-cut
;;     ;(global-set-key "\C-xm" 'browse-url-at-point)
;; (setq w3m-use-cookies t)
;; (setq w3m-coding-system 'utf-8
;;       w3m-file-coding-system 'utf-8
;;       w3m-file-name-coding-system 'utf-8
;;       w3m-input-coding-system 'utf-8
;;       w3m-output-coding-system 'utf-8
;;       w3m-terminal-coding-system 'utf-8)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /tabbar-ruler/
;; (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;; (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;; ;(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;; ;; (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;; ;; (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the scroll bar when your mouse is moving.

;; (require 'cl)
;; (require 'tabbar-ruler)

;; (global-set-key (kbd "C-c t") 'tabbar-ruler-move)
;; (define-key evil-normal-state-map "gT" 'tabbar-ruler-backward) ;prev tab
;; (define-key evil-normal-state-map "gt" 'tabbar-ruler-forward) ;next tab
;; (define-key evil-normal-state-map (kbd "C-w t") 'tabbar-ruler-up) ;up
;;   (defun kill-other-buffers ()
;;     "Kill all other buffers."
;;     (interactive)
;;     (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; (define-key evil-normal-state-map (kbd "C-w x") 'kill-this-buffer)
;; (define-key evil-normal-state-map (kbd "C-w X") 'kill-other-buffers)
;; ;; !! Remember the follow two keybindings CONFLICT with /org-mode/
;; ;; Failed in /org-mode/: default to "org-force-cycle-archived" & "..."
;; ;; (global-set-key [(control tab)] 'tabbar-ruler-forward)          ; eqiv. "gt"
;; ;; (global-set-key [(control shift tab)] 'tabbar-ruler-backward)     ; eqiv. "gT"

;; ;; hide for special buffers
;;   ;; special buffers that start with "*"
;; (setq tabbar-buffer-list-function
;;       (lambda ()
;;         (remove-if
;;          (lambda(buffer)
;;            (find (aref (buffer-name buffer) 0) " *"))
;;          (buffer-list))))
;;   ;; special buffers that user-defined
;; ;;  (setq *tabbar-ignore-buffers* '("BufferName 1" "BufferName 2" "BufferName 3"))
;; ;;  (setq tabbar-buffer-list-function
;; ;;        (lambda ()
;; ;;          (remove-if
;; ;;           (lambda (buffer)
;; ;;             (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
;; ;;                  (loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
;; ;;                        thereis (string-equal (buffer-name buffer) name))))
;; ;;           (buffer-list))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;; For /Speedbar/
;; ;; Disable hierarchy
;; (setq speedbar-tag-hierarchy-method nil)
;; ;; Show all files
;; (setq speedbar-show-unknown-files t)

;; ;; For /srSpeedbar/
;; ;; ; Change speedbar font size   "WenQuanYi Micro Hei Mono"
;;     ;; (defun y-speedbar-face ()
;;     ;;         (interactive)
;;     ;;         (setq buffer-face-mode-face '(:family "DejaVu Sans Mono" :height 92 :width semi-condensed :weight bold :slant normal))
;;     ;;         (buffer-face-mode))
;;     ;; (add-hook 'speedbar-mode-hook 'y-speedbar-face)
;; ;; ; Fix for emacs 24.4
;;     (defun ad-advised-definition-p (definition)
;;       "Return non-nil if DEFINITION was generated from advice information."
;;       (if (or (ad-lambda-p definition)
;;           (macrop definition)
;;           (ad-compiled-p definition))
;;           (let ((docstring (ad-docstring definition)))
;;         (and (stringp docstring)
;;              (get-text-property 0 'dynamic-docstring-function docstring)))))
;; (require 'sr-speedbar)
;; ;; (sr-speedbar-open)		; open speedbar by default
;; ;(setq speedbar-use-images nil)    ; use asicii symbols
;; (global-set-key (kbd "<f6>") 'sr-speedbar-toggle);; equiv matlab step in
;; ;; Fix width
;; ;; (with-current-buffer sr-speedbar-buffer-name
;; ;;    (setq window-size-fixed 'width))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;;; For /CEDET/
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (load-file "~/.emacs.d/git/cedet/cedet-devel-load.el")
;; (load-file "~/.emacs.d/git/cedet/contrib/cedet-contrib-load.el")

;; ;; ;; setting of /semantic/
;; ;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; ;(require 'semantic/ia)
;; ;; (require 'semantic/bovine/gcc)
;; ;; (defun oy-semantic-imenu-hook ()
;; ;;   (imenu-add-to-menubar "TAGS"))
;; ;; (add-hook 'semantic-init-hooks 'oy-semantic-imenu-hook)
;; ;; ;; ; enable support for gnu global
;; ;; ;; (when (cedet-gnu-global-version-check t)
;; ;; ;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;; ;; ;;   (semanticdb-enable-gnu-global-databases 'c++-mode))
;; ;; ;; ;; enable ctags
;; ;; ;; (when (cedet-ectag-version-check t)
;; ;; ;;   (semantic-load-enable-primary-ectags-support))
;; (add-hook 'c-mode-common-hook 'semantic-mode)
;; ;; ;(semantic-mode 1)

;; ;; ;; settings of /EDE/
;; ;; ;(global-ede-mode t)
;; ;; (add-hook 'c-mode-common-hook 'global-ede-mode)
;; ;; (ede-cpp-root-project "cproj"
;; ;;                 :name "cpp project"
;; ;;                 :file "~/Workspace/cpp/Makefile"
;; ;;                 :include-path '("/"
;; ;;                                 "/src"
;; ;;                                 "/lib"
;; ;;                                 "/common"
;; ;;                                )
;; ;;                 :system-include-path '("/usr/include")
;; ;;                 :spp-table '(("isUnix" . "")
;; ;;                              ("BOOST_TEST_DYN_LINK" . "")))
;; ;; ;; include symbol in boost
;; ;; (setq boost-base-dir "/usr/include/boost")
;; ;; (semantic-add-system-include boost-base-dir 'c++-mode)
;; ;; (add-to-list 'auto-mode-alist (cons boost-base-dir 'c++-mode))
;; ;; ;; include symbol in Qt4
;; ;; ;(setq qt4-base-dir "/usr/include/qt4")
;; ;; ;(semantic-add-system-include qt4-base-dir 'c++-mode)
;; ;; ;(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;; ;; ;(defvar semantic-lex-c-preprocessor-symbol-file '())
;; ;; ;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;; ;; ;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
;; ;; ;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

;; ;; ;; folding codes
;; ;; (load-library "contrib/semantic-tag-folding.el")
;; ;; (defun do-after-decorate () (semantic-tag-folding-mode t) )
;; ;; (add-hook 'semantic-decoration-mode-hook 'do-after-decorate)

;; ;; work with semantic
;; (require 'eassist)
;; (defun oy-eassist-cedet-hook ()
;;   (add-to-list 'ac-sources 'ac-source-gtags)
;;   (add-to-list 'ac-sources 'ac-source-semantic)
;;   ;; pressing bindng for "./>" not needed, having been enabled in auto-complete
;;   ;; (local-set-key "." 'semantic-complete-self-insert)
;;   ;; (local-set-key ">" 'semantic-complete-self-insert)
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c?" 'semantic-ia-fast-jump) ; return back by using /semantic-mrub-switch-tag/ or (C-x B)
;;   ;; (local-set-key "\C-c>" 'semantic-analyze-proto-impl-toggle) ; not work
;;   (local-set-key "\C-cv" 'semantic-decoration-include-visit)
;;   (local-set-key "\C-cr" 'semantic-symref-symbol)
;;   (local-set-key "\C-c\C-r" 'semantic-symref)
;; ;; usage of /semantic-symref popup window
;; ;    Tab         forward-button 可多按几次tab进行跳转，
;; ;    (           semantic-symref-list-create-macro-on-open-hit 这个不常用 ,跟宏录制相关
;; ;    +           semantic-symref-list-toggle-showing  toogle展开与否
;; ;    R           semantic-symref-list-rename-open-hits 可以进行批量重命名
;; ;    C-c C-e     semantic-symref-list-expand-all 展开所有，
;; ;    C-c C-r     semantic-symref-list-contract-all 折叠所有
;; ;    Enter       回车中转到相应代码处
;;   (local-set-key "\C-co" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-cm" 'eassist-list-methods)
;; ;; (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
;; ;; (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
;; ;; (local-set-key "\C-c\C-x+" 'semantic-tag-folding-show-all)
;; ;; (local-set-key "\C-c\C-x-" 'semantic-tag-folding-fold-all)
;; )
;; (add-hook 'c-mode-hook 'oy-eassist-cedet-hook)
;; (add-hook 'c++-mode-hook 'oy-eassist-cedet-hook)

;; ;; ;; ;; Turn on /sematic-mode/ /ede-mode/ in /matlab-mode/
;; ;; ;; (semantic-mode 1)
;; ;; ;;     ;; =======================================================================
;; ;; ;;     ;; !!! if want to use (matlab-cedet-setup), uncomment the next two lines.
;; ;; ;;     ; (add-hook 'matlab-mode-hook (lambda() (interactive) (semantic-mode 1)))
;; ;; ;;     ; (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))
;; ;; ;;     ;; =======================================================================


;; -----------------OBSOLETE------------------
;; Supports for tags
;; ;; For /built-in tags-update/, using =ctags -e=, NOT etags!
;; (defun oy-build-ctags ()
;;   (interactive)
;;   (message ">> building project tags ...")
;;   ;(let ((root "~/Workspace/"))
;;   (let ((root default-directory))
;;         (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " default-directory)))
;;   (oy-visit-project-tags)
;;   (message ">> tags built successfully!"))
;; (defun oy-visit-project-tags ()
;;   (interactive)
;;   (let ((tags-file (concat default-directory "TAGS")))
;;         (visit-tags-table tags-file)
;;         (message (concat "Loaded " tags-file))))
;; (global-set-key (kbd "C-c u") 'oy-build-ctags)
;; ;(define-key evil-normal-state-map (kbd "C-w u") 'oy-build-ctags) ;up
;; ;(evil-leader/set-key "up" 'oy-build-ctags)


;; ;; For /etags-table/
;; (require 'etags-table)
;; (setq etags-table-search-up-depth 10)
;; (setq tags-table-list '("~/Workspace/TAGS" "~/tmp/TAGS"))


;; ;; For /etags-select/
;; ;;;; [SOURCE]: https://github.com/emacsmirror/etags-select
;; (add-to-list 'load-path "~/.emacs.d/git/etags-select")
;; (load "etags-select.el")
;; (require 'etags-select)
;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; (global-set-key "\M-." 'etags-select-find-tag)
;; ;; Setting key bindings to close etags-select window
;; (define-key etags-select-mode-map (kbd "C-g") 'etags-select-quit)
;;         ;; Also quit etags-select when cursor moves to another window
;; (define-key etags-select-mode-map (kbd "C-x o") 'etags-select-quit)
;; ;; (define-key etags-select-mode-map (kbd "C-p") 'etags-select-previous-tag)
;; ;; (define-key etags-select-mode-map (kbd "C-n") 'etags-select-next-tag)
;; ;; default etags-select bindings
;;         ;; Return -> 'etags-select-goto-tag
;;         ;; M-Return -> 'etags-select-goto-tag-other-window
;;         ;; p -> 'etags-select-previous-tag
;;         ;; n -> 'etags-select-next-tag
;;         ;; q -> 'etags-select-quit
;;         ;; 0 -> (etags-select-by-tag-number "0")
;;         ;; 1 -> (etags-select-by-tag-number "1")
;;         ;; .. ..
;;         ;; 9 -> (etags-select-by-tag-number "9")




;; ----------------------------------------------------------------
;; Tested but NOT passed
;; ----------------------------------------------------------------
;; Integartion of /company/ and /yasnippet/
(global-set-key (kbd "C-<tab>") 'company-yasnippet-or-completion)
(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))
(add-hook 'company-mode-hook (lambda ()
  (substitute-key-definition 'company-complete-common
                             'company-yasnippet-or-completion
                             company-active-map)))
;; ----------------------------------------------------------------


;; ----------------------------------------------------------------
;; markdown
;; ----------------------------------------------------------------
(defun y:markdown-css-use-link-or-header (include-css)
  ;; nil: including css in headers; otherwise, use link.
  (if include-css
      (add-hook 'markdown-mode-hook (lambda()
        (add-to-list 'markdown-xhtml-header-content (concat css-default-path "style.css"))
        ;; (add-to-list 'markdown-xhtml-header-content
                     ;; (concat css-default-path "bootstrap.min_yeti.css"))
        ))
    (add-hook 'markdown-mode-hook (lambda()
      (add-to-list 'markdown-css-paths (concat css-default-path "style.css"))
      (add-to-list 'markdown-css-paths (concat css-default-path "bootstrap.min_yeti.css"))))))
(y:markdown-css-use-link-or-header 1)


;; ----------------------------------------------------------------
;; insert date
;; ----------------------------------------------------------------
;; simple formated dates
(defun insert-today (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d-%m-%Y")
                 ;; ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))
;; another version: formated string
(defun insdate-insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))
(defun insert-date (&optional days)
  "Insert date that is DAYS from current."
  (interactive "p*")
  (unless days (setq days 0))
  (insert
   (calendar-date-string
    (calendar-gregorian-from-absolute
     (+ (calendar-absolute-from-gregorian (calendar-current-date))
        days)))))

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")



;; ----------------------------------------------------------------
;; enhance colors for /company-mode/
;; ----------------------------------------------------------------
;; (when (eq 'solarized (car custom-enabled-themes))
;;   (set-face-attribute 'company-tooltip nil
;;                       :background "#fdf7e7")
;;   (set-face-attribute 'company-tooltip-common nil
;;                       :inherit font-lock-constant-face)
;;   (set-face-attribute 'company-tooltip-search nil
;;                       :foreground "#d33682")
;;   (set-face-attribute 'company-tooltip-search-selection nil
;;                       :inherit isearch)
;;   (set-face-attribute 'company-tooltip-selection nil
;;                       :inherit font-lock-function-name-face))



;; ----------------------------------------------------------------
;; frame size
;; ----------------------------------------------------------------
;(add-to-list 'default-frame-alist '(width . 96))
(when window-system (set-frame-size (selected-frame) 96 36))  ;(96,36) 33 in Thinkpad


;; ----------------------------------------------------------------
;; dired-x
;; ----------------------------------------------------------------
;; (add-hook 'dired-load-hook
;;           (lambda ()
;;             (require 'dired-x)
;;             (setq dired-omit-files "^\\.?#\\|^#.*\\|\\.DS_Store$\\|^Icon.*\\|\\..*\\.cache$\\|\\.git\\|\\.dropbox\\|\\.directory")
;;             (setq dired-omit-extensions (delete ".o" dired-omit-extensions))
;;             (setq dired-omit-extensions (delete ".so" dired-omit-extensions))
;;             (setq dired-omit-extensions (delete ".a" dired-omit-extensions))
;;             (setq dired-omit-extensions (append dired-omit-extensions '(".out" ".synctex.gz" ".url")))))



;; ----------------------------------------------------------------
;; Setting for /org-article/ for LaTeX  in Org-Mode
;; ----------------------------------------------------------------
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





;; ----------------------------------------------------------------
;; Adjust faces
;; ----------------------------------------------------------------

;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default ;:foreground "#3d3d3d"
;;                                    :background ,(color-lighten-name bg 1)))))
;;    `(company-tooltip-search-selection ((t (:inherit isearch))))
;;    `(company-tooltip-search ((t (:inherit default :foreground "#d33682"))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-annotation ((t (:inherit font-lock-comment-face
;;                                               :slant normal
;;                                               :background nil))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))



;; ----------------------------------------------------------------
;; Adjust faces
;; ----------------------------------------------------------------

(require 'color)
(defun y:fix-color-for-company-mode (&optional frame)
  ;; (select-frame frame)
  (when (or (eq 'monokai (car custom-enabled-themes))
            (eq 'solarized (car custom-enabled-themes))
            (eq 'atom-one-dark (car custom-enabled-themes)))
    (setq bg-color (face-attribute 'company-tooltip :background))
    (set-face-attribute 'company-tooltip nil :background
                        (color-lighten-name bg-color 2))
    (set-face-attribute 'company-tooltip-search nil :inherit 'font-lock-keyword-face)
    (set-face-attribute 'company-tooltip-search-selection nil :inherit 'isearch)
    (set-face-attribute 'company-tooltip-annotation nil
                        :inherit 'font-lock-comment-face :slant 'normal)
    (set-face-attribute 'company-tooltip-annotation-selection nil
                        :inherit 'font-lock-comment-face :slant 'normal)
    (set-face-attribute 'company-tooltip-common-selection nil
                       :inherit 'font-lock-function-name-face)
    (set-face-attribute 'company-tooltip-common nil :inherit 'font-lock-constant-face)))
;; use after-make-frame-functions hook to valid "emacs --daemon"
(require 'server)
(if (daemonp)
    (add-hook 'after-make-frame-functions 'y:fix-color-for-company-mode)
  (y:fix-color-for-company-mode))

;; ----------------------------------------------------------------
;; C/C++
;; ----------------------------------------------------------------
;; read in project-level include-paths via ".dir-locals.el"
;; an example of ".dir-locals.el":
;;    ((c++-mode . ((project-local-include-path . ("-I./include" "-I./src")))))
(defun y:readin-dir-local-path ()
  (cond ((boundp 'project-local-include-path)
         (setq ac-clang-cflags (append ac-clang-cflags project-local-include-path))
         (ac-clang-update-cmdlineargs))))
;; hook function defined generally to read in per-directory variables
(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)
(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
;; use for c/c++-mode to readin include-path defined under project roots
(add-hook 'c++-mode-local-vars-hook 'y:readin-dir-local-path)
(add-hook 'c-mode-local-vars-hook 'y:readin-dir-local-path)

;; Display Function Interface in the minibuffer (require semanticdb)
(require 'semantic)
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)
;; setting include paths
(semantic-add-system-include "/usr/include/c++/7.1.1" 'c++-mode)
(semantic-add-system-include "/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include" 'c-mode)
;; enable the function
(global-semantic-idle-summary-mode 1)


;; ----------------------------------------------------------------
;; fix $PATH for emacs in Mac OS X
(defun y-mac:set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    ;; (setenv "PATH" path-from-shell)
    (setenv "PATH" (concat "~/.emacs.d/bin:" "~/bin:" path-from-shell))
    (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages/")
    ;; (setq exec-path (split-string path-from-shell path-separator))
    (setq exec-path (split-string (getenv "PATH") path-separator))))
(defun y-linux:set-exec-path-from-shell-PATH()
  (setenv "PATH" (concat "~/.emacs.d/bin:" (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") path-separator)))
(when (string-equal system-type "darwin") (y-mac:set-exec-path-from-shell-PATH))
(when (string-equal system-type "gnu/linux") (y-linux:set-exec-path-from-shell-PATH))



;; ----------------------------------------------------------------
;; join mutiple lines in region (use "y:unfill-paragraph")
(defun y:join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))


;; ----------------------------------------------------------------
;; frame size
(set-frame-size (selected-frame)
                (cdr (assoc 'width default-frame-alist))
                (cdr (assoc 'height default-frame-alist)))


;; ----------------------------------------------------------------
(cond (*is-mac*
       (cond ((not (daemonp))
              (if window-system
                  (progn
                    (y:dired-open-folders-startup)
                    (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts")))
                nil))
             (*is-server-main*
              (progn
                (y:dired-open-folders-startup)
                (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts"))))
             (t (cd (expand-file-name "~/Public/Dropbox/Workspace/matlab")))))
      (*is-linux*
       (y:dired-open-folders-startup)
       (cd "~/tmp")))

;; ----------------------------------------------------------------
