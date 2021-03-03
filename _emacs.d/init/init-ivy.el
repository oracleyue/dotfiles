;; ===============================================================
;; Ivy - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 31 Mar 2018

;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------
(use-package counsel
  :demand
  :diminish (ivy-mode counsel-mode)
  :hook ((after-init . ivy-mode)
         (ivy-mode   . counsel-mode))
  :bind (([remap switch-to-buffer] . #'ivy-switch-buffer)
         ("C-c C-r"  . ivy-resume)
         :map ivy-minibuffer-map
         ("C-SPC"    . ivy-mark))  ;; use M-o to call action
  ;; counsel
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . find-library)
         ([remap imenu]                    . counsel-imenu)
         ([remap dired]                    . counsel-dired)
         ([remap recentf-open-files]       . counsel-recentf)
         ([remap insert-char]              . counsel-unicode-char)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap org-capture]              . counsel-org-capture)
         ;; minibuffer history
         ;; "C-r" ivy-reverse-i-search acts as counsel-minibuffer-history in counsel-mode
         ;; enchance built-in functions
         ("M-g u"   . counsel-unicode-char)
         ("M-g c"   . counsel-colors-web)
         ;; kill-ring
         ("M-y"     . counsel-yank-pop)
         ;; mark-ring
         ("M-g SPC" . counsel-mark-ring)
         ;; register
         ("M-g r"   . counsel-register)
         ;; bookmark (Emacs default; =C-x r b= to create bookmark)
         ("M-g b"   . counsel-bookmark)
         ;; recent files
         ;; ("M-g h"   . counsel-recentf)  ;; or use "counsel-recent-directory" defined later
         ;; code overview
         ("M-g i"   . counsel-semantic-or-imenu))
  ;; swiper
  :bind (("C-s"     . swiper)           ;; swiper-isearch
         ("s-f"     . swiper-isearch)
         ([remap swiper] . counsel-grep-or-swiper) ;; grep for large files
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ("M-g s"   . counsel-grep)     ;; using rg
         ;; all buffers
         ("C-S-s"   . swiper-all)
         ;; grep files recursively in the folder
         ("M-g a"   . counsel-rg)       ;; counsel-ag, counsel-ack, counsel-rg
         ;; git project
         ("C-x g"   . counsel-git)
         ("C-x s"   . counsel-git-grep)
         ;; system-wide files
         ("M-g f"   . counsel-fzf)      ;; find
         ("M-g M-l" . counsel-locate)
         :map swiper-map
         ("M-s"     . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-s"     . swiper-isearch-toggle))

  :init
  (setq enable-recursive-minibuffers t)

  ;; disable complete-symbol showing popup window at point; use minibuffer
  (setq ivy-display-functions-alist nil)

  (setq ivy-wrap   t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function       #'ivy-format-function-line
        ivy-use-virtual-buffers   t ;; add recent files to ivy-switch-buffer
        ivy-use-selectable-prompt t)  ;; make inputs selectable

  :config
  ;; auto prepend symbols in ivy commands
  ;; (setq ivy-initial-inputs-alist nil)  ; nothing
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . "^"))

  ;; number of items in completion list
  (push '(counsel-yank-pop . 15) ivy-height-alist)

  ;; file finding
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)\\|\\(.DS_Store\\)"
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-git-cmd "rg --files"
        counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
        ;; counsel-rg-base-command
        ;; "rg -zS --no-heading --line-number --color never %s ."
        ;; counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        ;; counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s"

  ;; fix the bug for ivy-occur in OSX
  (when *is-mac*
    (setq counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  ) ;; End of Ivy

;; ---------------------------------------------
;; Better sorting for Ivy candidates
;; ---------------------------------------------
(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :init
  (defun ivy-prescient-non-fuzzy (str)
    "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist
        '((counsel-ag . ivy-prescient-non-fuzzy)
          (counsel-rg . ivy-prescient-non-fuzzy)
          (counsel-pt . ivy-prescient-non-fuzzy)
          (counsel-grep . ivy-prescient-non-fuzzy)
          (counsel-imenu . ivy-prescient-non-fuzzy)
          (counsel-yank-pop . ivy-prescient-non-fuzzy)
          (swiper . ivy-prescient-non-fuzzy)
          (swiper-isearch . ivy-prescient-non-fuzzy)
          (swiper-all . ivy-prescient-non-fuzzy)
          (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
          (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
          (insert-char . ivy-prescient-non-fuzzy)
          (counsel-unicode-char . ivy-prescient-non-fuzzy)
          (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
               counsel-grep counsel-git-grep counsel-rg counsel-ag
               counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-org-capture counsel-load-theme counsel-yank-pop
               counsel-recentf counsel-buffer-or-recentf
               centaur-load-theme))

  (ivy-prescient-mode 1))

;; ---------------------------------------------
;; Hydra support for Ivy
;; ---------------------------------------------
(use-package ivy-hydra
  :commands ivy-hydra-read-action
  :init (setq ivy-read-action-function #'ivy-hydra-read-action))

;; ---------------------------------------------
;; Ivy integration for projectile
;; ---------------------------------------------
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input
              '(ivy-thing-at-point)))

;; ---------------------------------------------
;; Ivy integration for yasnippet
;; ---------------------------------------------
(use-package ivy-yasnippet
  :bind ("M-g y" . ivy-yasnippet))

;; ---------------------------------------------
;; Ivy integration for selecting xref candidates
;; ---------------------------------------------
(use-package ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; ---------------------------------------------
;; /Ivy-rich /: all-the-icons for Ivy interface
;; ---------------------------------------------
;; enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (icons-displayable-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

;; more friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

;; ---------------------------------------------
;; Use posframe for Ivy
;; ---------------------------------------------
(use-package ivy-posframe
  :demand
  :diminish
  :init
  (setq ivy-posframe-height 20
        ivy-posframe-parameters '((min-width  . 75)
                                  (min-height . 15)
                                  (internal-border-width . 10)))
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (ivy-completion-in-region . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1))

;; ---------------------------------------------
;; User-Extension: open recent directories
;; ---------------------------------------------
;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
;; https://emacs-china.org/t/topic/5948/3?u=et2010
(defvar counsel-recent-dir-selected nil)
(defvar counsel-recent-dir-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "TAB") 'counsel-recent-dir-find-file)
    (define-key map  [(tab)] 'counsel-recent-dir-find-file)
    map))

(defun counsel-recent-dir-find-file()
  (interactive)
  (ivy-exit-with-action
   (lambda (c)
     (setq counsel-recent-dir-selected c)
     (run-at-time 0.05 nil
                  (lambda()
                    (let ((default-directory counsel-recent-dir-selected))
                      (counsel-find-file)))))))

(defun counsel-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string
                       (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :keymap counsel-recent-dir-map
              :action (lambda (x) (if (fboundp 'ranger) (ranger x) (dired x))))))

(global-set-key (kbd "M-g h") 'counsel-recent-directory)

;; ---------------------------------------------
;; /counsel-gtags/: Ivy for gtags (GNU global)
;; ---------------------------------------------
(use-package counsel-gtags
  :ensure nil
  :load-path "site-lisp/"
  :init
  (setq counsel-gtags-auto-update t
        counsel-gtags-custom-dbpath ".tags/")
  :bind-keymap ("C-c g" . counsel-gtags-command-map)
  ;; basic jumps
  ;; ("C-c g ." . counsel-gtags-dwim)
  ;; ("C-c g ," . counsel-gtags-go-backward)
  ;; ("C-c g d" . counsel-gtags-find-definition)
  ;; ("C-c g r" . counsel-gtags-find-reference)
  ;; ("C-c g s" . counsel-gtags-find-symbol)
  ;; ("C-c g f" . counsel-gtags-find-file)
  ;; create/update tags
  ;; ("C-c g c" . counsel-gtags-create-tags)
  ;; ("C-c g u" . counsel-gtags-update-tags)
  ;; go through stack/history
  ;; ("C-c g n" . counsel-gtags-go-forward)
  ;; ("C-c g p" . counsel-gtags-go-backward)
  :bind (:map counsel-gtags-mode-map
              ("s-."     . counsel-gtags-dwim)
              ("s-,"     . counsel-gtags-go-backward))
  :hook ((c-mode c++-mode matlab-mode) . counsel-gtags-mode))
;; If you like to skip folders for tagging, add folders to the list
;; ":skip=" in ~/.globalrc.

;; ---------------------------------------------------------------
;; Ivy for Dash (Mac only, requires Dash and Alfred)
;; ---------------------------------------------------------------
(use-package ivy-dash
  :ensure nil
  :if *is-mac*
  :load-path "site-lisp"
  :bind ("M-s s"   . dash-in-ivy))


(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
