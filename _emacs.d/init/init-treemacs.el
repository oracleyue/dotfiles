;; ================================================================
;; Treemacs: A tree layout file explorer.
;; ================================================================

;; Install required Emacs packages
;; (setq custom/treemacs-packages
;;       '(treemacs
;;         treemacs-projectile
;;         treemacs-magit))
;; (custom/install-packages custom/treemacs-packages)


(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
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
        treemacs-width                         30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; Projectile integration
  (use-package treemacs-projectile
    :after projectile
    :bind (([M-f8] . treemacs-projectile)
           :map projectile-command-map
           ("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage) . treemacs-magit--schedule-update)))


(provide 'init-treemacs)
;; ================================================
;; init-treemacs.el ends here
