;; ================================================================
;; Private Settings
;; ================================================================

;; [Warning]: You shouldn't load this file, since it is highly specialized
;; for my own computer, such personal folders, files, etc.


;; open default folders/files on startup
(cond
 ((or *is-server-main* *is-app*)
  (dired (expand-file-name "~/Public/Dropbox/Academia"))
  (find-file (expand-file-name "~/Public/Dropbox/Academia/ToDoList.org"))
  (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts")))
 (*is-server-coding*
  (cd (expand-file-name "~/Public/Dropbox/Workspace/Matlab"))))

;; switch to default buffer
(unless (get-buffer "*dashboard*")
  (switch-to-buffer "*scratch*"))


(provide 'init-private)
;; ================================================
;; init-private.el ends here
