;; ================================================================
;; Configure Splash/Startup Screen
;; ================================================================
;; Last modified on 02 Oct 2019

(use-package dashboard
  :after nerd-icons
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (add-hook 'window-setup-hook (lambda () (dashboard-refresh-buffer)))
  :bind (:map dashboard-mode-map
              ("h" . widget-backward)
              ("l" . widget-forward)
              ("q" . quit-window))
  :config
  ;; (dashboard-setup-startup-hook)
  ;; basic
  (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
        dashboard-startup-banner (or zyue-logo 'official))
  (setq dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts nil)
  ;; icons
  (setq dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons    t)
  ;; items
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((projects  . 5)
                          (bookmarks . 5)
                          (recents   . 5)))
  (dashboard-modify-heading-icons '((projects  . "nf-oct-rocket")
                                    (recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")))
  ;; footer
  (setq dashboard-set-footer t
        dashboard-footer-messages
        (list (format "Powered by oracleyue, %s" (format-time-string "%Y")))
        dashboard-footer-icon (nerd-icons-mdicon "nf-md-heart" :face 'nerd-icons-red))

  ;; navigator
  (setq dropbox-root (expand-file-name "Public/Dropbox" "~"))
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-sucicon "nf-custom-emacs")
            "Emacs" "Browse .emacs.d/init"
            (lambda (&rest _) (dired user-emacs-directory)))
           (,(nerd-icons-mdicon "nf-md-folder_file")
            "Academia" "Browse ..Dropbox/Academia"
            (lambda (&rest _) (dired (expand-file-name "Academia" dropbox-root))))
           (,(nerd-icons-octicon "nf-oct-code")
            "Workspace" "Browse ..Dropbox/Workspace"
            (lambda (&rest _) (dired (expand-file-name "Workspace" dropbox-root))))
           (,(nerd-icons-faicon "nf-fa-dropbox")
            "Notebooks" "Browse .org/.md notes"
            (lambda (&rest _) (zyue/deft)))
           )))

  ;; setup layout
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    ;; dashboard-insert-init-info
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  ;; fix: default all fonts in bold
  (set-face-attribute 'dashboard-items-face nil :weight 'normal)
  (set-face-attribute 'dashboard-heading-face nil :weight 'bold)
  ) ;END of use-package


(provide 'init-dashboard)
;; ================================================================
;; init-dashboard.el ends here
