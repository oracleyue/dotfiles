;; ===============================================================
;; Ivy - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 31 Mar 2018

;; Install required Emacs packages
(setq custom/ivy-packages
      '(ivy
        counsel
        swiper
        ivy-posframe
        wgrep
        counsel-projectile
        counsel-gtags
        hydra
        ivy-hydra
        avy))
(custom/install-packages custom/ivy-packages)

;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------
(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function #'ivy-format-function-line
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t  ;; make inputs selectable
        )
  ;; disable popup windows for completion-at-point; use minibuffer
  (setq ivy-display-functions-alist nil)
  ;; enable Ivy
  (ivy-mode +1)
  :bind (([remap switch-to-buffer] . #'ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  )

(when *use-posframe*
  (use-package ivy-posframe
    :load-path "git/ivy-posframe"
    :after (ivy)
    :config
    (setq ivy-fixed-height-minibuffer nil
          ;; ivy-display-function #'ivy-posframe-display-at-point
          ivy-posframe-parameters
          `((min-width . 90)
            (min-height . ,ivy-height)
            (internal-border-width . 10)))
    (setq ivy-display-functions-alist nil)
    (push '(t . ivy-posframe-display-at-point) ivy-display-functions-alist)
    ;; (push '(ivy-completion-in-region . ivy-posframe-display-at-point)
    ;;       ivy-display-functions-alist)
    (push '(swiper . ivy-posframe-display-at-window-bottom-left)
          ivy-display-functions-alist)
    (ivy-posframe-enable))
  )

(use-package counsel
  :after (ivy)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . find-library)
         ([remap imenu]                    . counsel-imenu)
         ([remap recentf-open-files]       . counsel-recentf)
         ([remap org-capture]              . counsel-org-capture)
         ([remap swiper]                   . counsel-grep-or-swiper) ;; large files
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ;; completion
         ;; ([remap completion-at-point]      . counsel-company)
         ;; editing and code overview
         ("M-y"     . counsel-yank-pop)
         ("M-g SPC" . counsel-mark-ring)
         ("M-g i"   . counsel-semantic-or-imenu)
         ;; system tools
         ("M-g f"   . counsel-fzf)     ;; find
         ("M-g l"   . counsel-locate)  ;; locate
         )

  :config
  (push '(counsel-yank-pop . 10) ivy-height-alist)
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

  ;; minibuffer actions
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; ensure recentf-list loaded on startup
  (with-eval-after-load 'counsel (recentf-mode))
  ;; disable recentf-list loading via ivy-switch-buffer
  ;; (setq recentf-initialize-file-name-history nil)

  ;; fix the bug for ivy-occur in OSX
  (when *is-mac*
    (setq counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  )

(use-package swiper
  :defer 1
  :bind
  (;; buffer
   ("C-s"   . swiper)
   ("C-S-s" . swiper-all)
   ("s-f"   . swiper-isearch)   
   ("M-g s" . counsel-grep)
   ;; git project
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)  ;; use counsel-rg instead
   ;; bookmark (Emacs default; =C-x r b= to create bookmark)
   ("M-g b" . counsel-bookmark)
   ;; grep files recursively in the folder
   ("M-g a" . counsel-ag)    ;; C-c k
   ("M-g k" . counsel-ack)
   ("M-g r" . counsel-rg)))

;; use ivy to open recent directories
;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
;; https://emacs-china.org/t/topic/5948/3?u=et2010
(defvar counsel-recent-dir-selected "~/")

(defvar counsel-recent-dir-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "TAB") 'counsel-recent-dir-find-file)
    (define-key map  [(tab)] 'counsel-recent-dir-find-file)
    map))

(defun counsel-recent-dir-find-file()
  (interactive)
  (ivy-exit-with-action
   (lambda(c)
     (setq counsel-recent-dir-selected c)
     (run-at-time 0.05 nil
                  (lambda()
                    (let ((default-directory counsel-recent-dir-selected))
                      ;; (find-file counsel-recent-dir-selected)
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
;; /counsel-projectile/: Ivy for projectile
;; ---------------------------------------------
(use-package counsel-projectile
  :after projectile
  :commands (counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-to-buffer
             counsel-projectile-grep
             counsel-projectile-ag
             counsel-projectile-switch-project)
  :init
  :bind
  (([remap projectile-find-file]        . counsel-projectile-find-file)
   ([remap projectile-find-dir]         . counsel-projectile-find-dir)
   ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
   ([remap projectile-grep]             . counsel-projectile-grep)
   ([remap projectile-ag]               . counsel-projectile-ag)
   ([remap projectile-switch-project]   . counsel-projectile-switch-project)))

;; ---------------------------------------------
;; /counsel-gtags/: Ivy for gtags (GNU global)
;; ---------------------------------------------
(use-package counsel-gtags
  :bind (:map counsel-gtags-mode-map
              ;; basic jumps
              ("C-c g ." . counsel-gtags-dwim)
              ("C-c g ," . counsel-gtags-go-backward)
              ("C-c g t" . counsel-gtags-find-definition)
              ("C-c g r" . counsel-gtags-find-reference)
              ("C-c g s" . counsel-gtags-find-symbol)
              ;; create/update tags
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)
              ;; jump over stacks/history
              ("C-c g [" . counsel-gtags-go-backward)
              ("C-c g ]" . counsel-gtags-go-forward))
  :hook ((c-mode c++-mode python-mode matlab-mode) . counsel-gtags-mode)
  ;; :config (setq counsel-gtags-auto-update t)
  )

;; ---------------------------------------------------------------
;; Hydra: make Emacs bindings that stick around
;; ---------------------------------------------------------------
(use-package hydra)

;; ---------------------------------------------------------------
;; Avy: jump to char/words in tree-style
;; ---------------------------------------------------------------
(use-package avy
  :bind (("C-'"     . avy-goto-char)   ;; C-:
         ("M-'"     . avy-goto-char-2) ;; C-'
         ("M-g c"   . avy-goto-char)
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ;; ("M-g e"   . avy-goto-word-0)  ;; too many candiates
         ("M-g M-r" . avy-resume))
  :config
  (avy-setup-default)
  )

;; ---------------------------------------------------------------
;; Ivy-based Packages (mini-functions)
;; ---------------------------------------------------------------

;; Ivy-based Dash search
(use-package ivy-dash
  :load-path "git"
  :bind (("M-g d" . dash-in-ivy)))

(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
