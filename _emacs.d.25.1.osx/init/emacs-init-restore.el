;;
;; ------------- Restore Original Settings ---------------
;;

;; restore "M-c" for capitalize words
(global-set-key (kbd "M-c") 'capitalize-word)

;; open default Dired folders on startup
(cond ((string-equal system-type "darwin")
       (if (or (string-equal "main" (daemonp)) (not (daemonp)))
           (progn
             (y:dired-open-folders-startup)  ; defined in emacs-init-basics.el
             (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts")))
         (cd "/Users/oracleyue/Public/Dropbox/Workspace/matlab")))  ; for dark-version
      ((string-equal system-type "gnu/linux")
       (y:dired-open-folders-startup)  ; defined in emacs-init-basics.el
       (cd "~/tmp")))

;; restore "F10" to open menu bar or "M-`" for terminal
(when (not (display-graphic-p))
  (global-set-key (kbd "<f10>") 'menu-bar-open))



(provide 'emacs-init-restore)
;; ================================================
;; emacs-init-restore.el ends here