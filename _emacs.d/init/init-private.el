;; ================================================================
;; Private Settings
;; ================================================================

;; [Warning]: You shouldn't load this file, since it is highly specialized
;; for my own computer, such personal folders, files, etc.


;; open default folders/files on startup
(setq dropbox-path "~/Public/Dropbox")
(cond
 ((or *is-server-main* *is-app*)
  (dired (expand-file-name "Academia" dropbox-path))
  (find-file (expand-file-name "oracleyue/OrgAgenda/ToDoList.org" dropbox-path))
  (cd (expand-file-name "Academia/Manuscripts" dropbox-path)))
 (*is-server-coding*
  (cd (expand-file-name "Workspace/Matlab" dropbox-path))))

;; switch to default buffer
(unless (get-buffer "*dashboard*")
  (switch-to-buffer "*scratch*"))


(provide 'init-private)
;; ================================================
;; init-private.el ends here
