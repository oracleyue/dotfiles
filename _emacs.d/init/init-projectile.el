;; ===============================================================
;; Project Management via /projectile/
;; ===============================================================
;; Last modified on 02 Mar 2021

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-p"   . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order       'recentf
        projectile-use-git-grep     t)

  :config
  ;; Ignore folders or files
  (setq projectile-ignored-projects '("~/"
                                      "~/Public/Dropbox/"))
  ;; Due to "alien" indexing method, globally ignore folders/files by
  ;; re-defining "rg" args
  (mapc (lambda (item)
          (add-to-list 'projectile-globally-ignored-directories item))
        '("Backup" "backup" "auto" "archived"))
  ;; files to be ignored should be listed in "~/.emacs.d/templates/rg_ignore"

  ;; Control project root additional guess (besides ".git", ".projectile")
  (setq projectile-project-root-files
        '("CMakeLists.txt" "Makefile" "configure.ac" "configure.in"
          "TAGS" "GTAGS"))

  ;; Use the faster searcher to handle project files: ripgrep "rg"
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
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
