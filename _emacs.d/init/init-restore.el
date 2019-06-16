;; ================================================================
;; Restore Original Settings
;; ================================================================


;; restore "M-c" for capitalize words
(global-set-key (kbd "M-c") 'capitalize-word)

;; unset keys
(global-unset-key (kbd "s-k"))  ;; =super-k= kill current buffer

;; open default Dired folders on startup
(cond (*is-mac*
       (cond (*is-server-main*
              (progn
                (zyue/dired-open-folders-startup)
                (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts"))))
             (*is-server-coding*
              (cd (expand-file-name "~/Public/Dropbox/Workspace/matlab")))))
      (*is-linux*
       (zyue/dired-open-folders-startup)
       (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts"))))

;; restore keybindings for emacs in terminal
(when *is-terminal*
  ;; "F10" to open menu bar or "M-`"
  (global-set-key (kbd "<f10>") 'menu-bar-open)
  ;; fix line numbering in ssh terminal
  (setq linum-format "%4d "))



(provide 'init-restore)
;; ================================================
;; init-restore.el ends here
