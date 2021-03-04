;; ===============================================================
;; Project Management via /projectile/
;; ===============================================================
;; Last modified on 02 Mar 2021

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t"   . projectile-find-file)
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order       'recentf
        projectile-use-git-grep     t)
  :config
  ;; (projectile-update-mode-line)    ; update mode-line at the first time

  ;; Ignore strange project roots
  (setq projectile-ignored-projects '("~/" "~/Public/Dropbox/"))

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when *is-win*
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))
    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))
  )


(provide 'init-projectile)
;; ================================================
;; init-projectile.el ends here
