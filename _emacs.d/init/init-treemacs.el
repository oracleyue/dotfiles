;; ================================================================
;; Treemacs: A tree-like file explorer.
;; ================================================================
;; Last modified on 13 Nov 2024

;; Usage:
;; - "C-c p h": add projectile project into treemacs
;; - "M-0" or "C-c t t": open or close treemacs sidebar
;; - editing in the sidebar:
;;   - "C-c C-w a": treemacs-create-workspace
;;   - "C-c C-w d": treemacs-remove-workspace
;;   - "C-c C-w e": treemacs-edit-workspaces

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :init (setq treemacs-no-load-time-warnings t)  ;; suppress annoying warnings
  :bind (("M-0"       . treemacs-select-window)  ;; treemacs
         ("C-x t t"   . treemacs)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t d"   . treemacs-select-directory)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         ;; TAB on file in treemacs to see tags
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-sorting                       'alphabetic-asc
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

;; Icon supports for treemacs (default using PNG images from treemacs)
(if (string= *icons-type* "nerd-icons")
    ;; use /nerd-icons/
    (use-package treemacs-nerd-icons
      :demand
      :config (treemacs-load-theme "nerd-icons"))
  ;; use /all-the-icons/
  ;; (note: doom-themes supports all-the-icons in its own way.)
  (use-package treemacs-all-the-icons
    :after (treemacs all-the-icons)
    :demand
    :config (treemacs-load-theme "all-the-icons")))


(provide 'init-treemacs)
;; ================================================
;; init-treemacs.el ends here
