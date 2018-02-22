;;
;; ------------- Restore Original Settings ---------------
;;

;; restore "M-c" for capitalize words
(global-set-key (kbd "M-c") 'capitalize-word)

;; open default Dired folders on startup
(cond ((string-equal system-type "darwin")
       (cond ((not (daemonp))
              (if window-system
                  (progn
                    (y:dired-open-folders-startup)
                    (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts")))
                nil))
             ((string-equal (daemonp) "main")
              (progn
                (y:dired-open-folders-startup)
                (cd (expand-file-name "~/Public/Dropbox/Academia/Manuscripts"))))
             (t (cd (expand-file-name "~/Public/Dropbox/Workspace/matlab")))))
      ((string-equal system-type "gnu/linux")
       (y:dired-open-folders-startup)
       (cd "~/tmp")))

;; restore keybindings for emacs in terminal
(when (not (display-graphic-p))
  ;; "F10" to open menu bar or "M-`"
  (global-set-key (kbd "<f10>") 'menu-bar-open)
  ;; fix "TAB" for code completion in cc-mode (or use "M-TAB")
  (define-key c++-mode-map (kbd "TAB") 'tab-indent-or-complete)
  ;; fix line numbering in ssh terminal
  (setq linum-format " %d  "))



(provide 'emacs-init-restore)
;; ================================================
;; emacs-init-restore.el ends here