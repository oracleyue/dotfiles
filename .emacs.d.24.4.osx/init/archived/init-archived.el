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