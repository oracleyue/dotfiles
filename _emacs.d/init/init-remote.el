;; ================================================================
;; Remote Access in Emacs
;; ================================================================
;; Last modified on 02 Mar 2021

;; ------------------------------------------------
;; /TRAMP/: manage ssh and remote access
;; ------------------------------------------------
(setq tramp-default-method "ssh")
;; usages:
;; - "C-x C-f /ssh:gaia:/home/users/zuogong.yue/..." or without "ssh:"
;; - "C-x C-f /sudo::/etc/hosts"


(provide 'init-remote)
;; ================================================
;; init-remote.el ends here
