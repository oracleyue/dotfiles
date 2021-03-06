;; ================================================================
;; Treemacs: A tree-like file explorer.
;; ================================================================
;; Last modified on 03 Mar 2021

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("C-x t t"   . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-sorting                       'alphabetic-case-insensitive-desc
        treemacs-follow-after-init             t
        treemacs-is-never-other-window         t
        treemacs-silent-filewatch              t
        treemacs-silent-refresh                t
        treemacs-width                         30
        treemacs-no-png-images                 *is-terminal*)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  ) ; END of treemacs

;; Integration for /projectile/
(use-package treemacs-projectile
  :after (treemacs projectile)
  :bind (([M-f8] . treemacs-projectile)
         :map projectile-command-map
         ("h" . treemacs-projectile)))

;; Integration for /magit/
(use-package treemacs-magit
  :after (treemacs magit)
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage) . treemacs-magit--schedule-update))

;; Support all-the-icons in treemacs (default using PNG images)
;; Note: doom-themes supports all-the-icons in its own way.
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :demand
  :if (and *enable-all-the-icons* *is-graphic*
           (not (member zyue-theme '(eclipse))))
  :config
  (treemacs-load-theme "all-the-icons"))


(provide 'init-treemacs)
;; ================================================
;; init-treemacs.el ends here
