;; ================================================================
;; Configure Splash/Startup Screen
;; ================================================================
;; Last modified on 02 Oct 2019


(use-package dashboard
  :diminish dashboard-mode
  :init (dashboard-setup-startup-hook)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :bind (:map dashboard-mode-map
              ("q" . quit-window))
  :config
  (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
        dashboard-startup-banner (or zyue-logo 'official)
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-set-init-info t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database")))
  (require 'all-the-icons)
  (setq dashboard-set-footer t
        dashboard-footer (format "Powered by oracleyue, %s" (format-time-string "%Y"))
        dashboard-footer-icon
        (all-the-icons-faicon "heart" :height 1.1 :v-adjust -0.05 :face 'error))
  )


(provide 'init-dashboard)
;; ================================================================
;; init-dashboard.el ends here
