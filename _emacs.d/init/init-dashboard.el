;; ================================================================
;; Configure Splash/Startup Screen
;; ================================================================
;; Last modified on 02 Oct 2019


(use-package dashboard
  :diminish page-break-lines-mode
  :after    all-the-icons
  :init
  (dashboard-setup-startup-hook)
  (set-face-attribute 'dashboard-items-face nil :weight 'normal)
  :custom-face
  (dashboard-heading    ((t (:inherit (font-lock-string-face bold)))))
  :bind (:map dashboard-mode-map
              ("h" . widget-backward)
              ("l" . widget-forward)
              ("q" . quit-window))
  :config
  (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
        dashboard-startup-banner (or zyue-logo 'official)
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((bookmarks . 4)
                          (projects  . 4)
                          (recents   . 4))
        dashboard-set-init-info t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database")))

  (setq dashboard-set-footer t
        dashboard-footer-messages (list (format "Powered by oracleyue, %s" (format-time-string "%Y")))
        dashboard-footer-icon
        (all-the-icons-faicon "heart" :height 1.1 :v-adjust -0.05 :face 'error))

  (setq dashboard-set-navigator t
        dropbox-root (expand-file-name "Public/Dropbox" "~")
        dashboard-navigator-buttons
        `(((,(when *is-graphic*
               (all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1))
            "Emacs" "Browse .emacs.d/init"
            (lambda (&rest _) (dired user-emacs-directory)))
           (,(when *is-graphic*
               (all-the-icons-faicon "dropbox" :height 1.0 :v-adjust 0.0))
            "Academia" "Browse ..Dropbox/Academia"
            (lambda (&rest _) (dired (expand-file-name "Academia" dropbox-root))))
           (,(when *is-graphic*
               (all-the-icons-faicon "code" :height 1.0 :v-adjust 0.0))
            "Workspace" "Browse ..Dropbox/Workspace"
            (lambda (&rest _) (dired (expand-file-name "Workspace" dropbox-root))))
           (,(when *is-graphic*
               (all-the-icons-fileicon "org" :height 1.0 :v-adjust -0.1))
            "Notebooks" "Browse .org notebooks"
            (lambda (&rest _) (deft)))
           (,(if *is-graphic*
                 (all-the-icons-faicon "question" :height 1.0 :v-adjust -0.1)
               "?")
            "" "Help (?)"
            (lambda (&rest _) (fancy-about-screen))
            font-lock-string-face))))

  ) ;END of use-package


(provide 'init-dashboard)
;; ================================================================
;; init-dashboard.el ends here
