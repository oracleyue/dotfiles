;;
;; ------------- Restore Original Settings ---------------
;;

;; restore "M-c" for capitalize words
(global-set-key (kbd "M-c") 'capitalize-word)

;; open default Dired folders on startup
(cond (*is-mac*
       (cond (*is-server-main*
              (progn
                (zyue/dired-open-folders-startup)
                (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts"))))
             ((or *is-server-coding* *is-server-ac*)
              (cd (expand-file-name "~/Public/Dropbox/Workspace/matlab")))))
      (*is-linux*
       (zyue/dired-open-folders-startup)
       (cd "~/tmp")))

;; restore keybindings for emacs in terminal
(when (and (not (display-graphic-p)) (not (daemonp)))
  ;; "F10" to open menu bar or "M-`"
  (global-set-key (kbd "<f10>") 'menu-bar-open)
  ;; fix "TAB" for code completion in cc-mode (or use "M-TAB")
  (define-key c++-mode-map (kbd "TAB") 'tab-indent-or-complete)
  ;; fix line numbering in ssh terminal
  (setq linum-format "%4d "))



(provide 'init-restore)
;; ================================================
;; init-restore.el ends here
