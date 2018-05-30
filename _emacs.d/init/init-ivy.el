;; ===============================================================
;; Ivy - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 31 Mar 2018

;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b"   . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-height 15)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)  ;; make inputs selectable
)

(use-package counsel
  :ensure t
  :bind
  (;; basics
   ("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> l"  . counsel-find-library)
   ("<f2> k"  . counsel-descbinds)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ;; editing and code overview
   ("M-y"     . counsel-yank-pop)
   ("M-g SPC" . counsel-mark-ring)
   ("M-g i"   . counsel-semantic-or-imenu)
   ;; system tools
   ("M-s f"   . counsel-fzf)     ;; find
   ;; ("M-s l"   . counsel-locate)  ;; locate
  )
  :config
  (setq counsel-find-file-at-point t)
  ;; tools for grep
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  ;; minibuffer actions
  (define-key minibuffer-local-map (kbd "C-r")
    'counsel-minibuffer-history)
  ;; ensure recentf-list loaded on startup
  (with-eval-after-load 'counsel (recentf-mode))
  ;; disable recentf-list loading via ivy-switch-buffer
  ;; (setq recentf-initialize-file-name-history nil)

  ;; exclude boring files as =.DS_Store=
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\'")

  ;; fix the bug for ivy-occur in OSX
  (when *is-mac*
    (setq counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  )

(use-package swiper
  :ensure t
  :after (ivy counsel)
  :bind
  (("C-s"   . swiper)
   ("C-S-s" . swiper-all)   
   ;; ("C-s"   . counsel-grep-or-swiper)  ;; alternative for large files
   ("C-c g" . counsel-git)
   ;; ("C-c j" . counsel-git-grep)  ;; use counsel-rg instead
   ("M-s s" . counsel-grep)  ;; grep the current file
   ;; grep files recursively in the folder
   ("M-s a" . counsel-ag)    ;; C-c k
   ("M-s k" . counsel-ack)
   ("M-s r" . counsel-rg))
  :config
  (setq counsel-git-cmd "rg --files")
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
  )

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
  :requires projectile
  :config
  (counsel-projectile-mode))

;; ---------------------------------------------
;; /counsel-gtags/: Ivy for gtags (GNU global)
;; ---------------------------------------------
(use-package counsel-gtags
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (add-hook 'python-mode-hook 'counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
              ;; basic jumps
              ("M-." . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ;; create/update tags
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)
              ;; jump over stacks/history
              ("C-c g [" . counsel-gtags-go-backward)
              ("C-c g ]" . counsel-gtags-go-forward))
  )

;; ---------------------------------------------------------------
;; Hydra: make Emacs bindings that stick around
;; ---------------------------------------------------------------
(use-package hydra
  :ensure t :ensure ivy-hydra)

;; ---------------------------------------------------------------
;; Avy: jump to char/words in tree-style
;; ---------------------------------------------------------------
(use-package avy
  :ensure t
  :bind (("C-'"   . avy-goto-char)   ;; C-:
         ("M-'"   . avy-goto-char-2) ;; C-'
         ("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ;; ("M-g e" . avy-goto-word-0)  ;; too many candiates
         ("M-g M-r" . avy-resume))
  :config
  (avy-setup-default)
  )

(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
