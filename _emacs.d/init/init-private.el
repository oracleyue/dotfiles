;; ================================================================
;; Private Settings
;; ================================================================
;; [Warning]: You shouldn't load this file, since it is highly specialized
;; for my own computer, such personal folders, files, etc.

;; open default folders/files on startup
(setq dropbox-path "~/Public/Dropbox")
(cond
 (*is-server-m*
  (dired (expand-file-name "Academia" dropbox-path))
  (dired (expand-file-name "oracleyue/GTD" dropbox-path))
  (dired (expand-file-name "oracleyue/Notebooks/Research" dropbox-path))
  ;; (find-file todo-file)
  )
 (*is-server-c*
  (cd (expand-file-name "Workspace" dropbox-path))))

;; switch to default buffer
(unless (get-buffer "*dashboard*")
  (switch-to-buffer "*scratch*"))


(provide 'init-private)
;; ================================================
;; init-private.el ends here
