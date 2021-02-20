;; ===============================================================
;; Project Management
;; ===============================================================


;; ---------------------------------------------
;; /projectile/: project management
;; ---------------------------------------------
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t"   . projectile-find-file)
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)    ; Update mode-line at the first time

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

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Do not visit the current project's tags table if `helm-projectile-mode' or
  ;; 'counsel-projectile-mode' is loaded.  Doing so prevents the unnecessary call
  ;; to `visit-tags-table' function and the subsequent `find-file' call for the
  ;; `TAGS' file."
  (defun zyue/projectile-dont-visit-tags-table ()
    "Don't visit the tags table as we are using gtags/global."
    nil)
  (when (or (fboundp 'helm-gtags) (fboundp 'counsel-gtags-mode))
    (advice-add 'projectile-visit-project-tags-table :override
                #'zyue/projectile-dont-visit-tags-table))
  )

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


(provide 'init-projectile)
;; ================================================
;; init-projectile.el ends here
