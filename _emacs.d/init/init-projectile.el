;; ===============================================================
;; Project Management via /projectile/
;; ===============================================================
;; Last modified on 13 Nov 2024

;; Notes:
;; - Create .projectile or .gitignore or .git/ to create make a project.
;; - Use "C-c p p" to switch project; "C-c p f" to find files in the project
;; - To ignore files/folders,
;;   - ".projectile" only works for "native" indexing;
;;   - we use "ripgrep" for "alien" indexing, which only obeys ".gitignore".
;; - When caching enabled, use "C-c p i" to fore re-caching.

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-p"   . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c p C-a" . projectile-add-known-project)
              ("C-c p C-d" . projectile-remove-known-project))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-indexing-method  'alien
        projectile-enable-caching   t
        ;; note: force to re-caching: "C-c p i" or "projectile-invalidate-cache"
        projectile-sort-order       'recentf
        ;; projectile-use-git-grep     t
        )

  :config
  ;; Ignore folders or files
  (setq projectile-ignored-projects '("~/" "~/tmp/"
                                      "~/Public/Dropbox/"))

  ;; Project root additional guess (besides ".git", ".projectile")
  (setq projectile-project-root-files
        '("CMakeLists.txt" "Makefile" ".gitignore"))

  ;; Re-defining "rg" args to globally ignore folders/files in "alien" indexing
  ;; Note: files to be ignored should be listed in "~/.emacs.d/templates/rg_ignore"
  (mapc (lambda (item)
          (add-to-list 'projectile-globally-ignored-directories item))
        '("auto" "archived" "backup"))
  ;; Use the faster searcher to handle project files: ripgrep "rg"
  (when (executable-find "rg")
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (setq rg-ignorefile
                  (concat "--ignore-file" " "
                          (expand-file-name "templates/rg_ignore" user-emacs-directory)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd " " rg-ignorefile))))

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
