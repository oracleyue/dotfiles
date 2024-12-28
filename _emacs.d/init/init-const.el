;; ================================================================
;; Select or Enable Features
;; ================================================================

;; versions
(defconst emacs/>=26p (>= emacs-major-version 26) "Emacs is 26 or above.")
(defconst emacs/>=27p (>= emacs-major-version 27) "Emacs is 27 or above.")

;; system and app/console
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-win* (string-equal system-type "windows-nt"))

(defconst *is-app* (and (display-graphic-p) (not (daemonp))))
(defconst *is-server-m* (string-equal "main" (daemonp)))
(defconst *is-server-c* (string-equal "coding" (daemonp)))
(defconst *is-server-t* (string-equal "tty" (daemonp)))
(defconst *is-graphic*  (or *is-app* *is-server-m* *is-server-c*))
(defconst *is-terminal* (or (not *is-graphic*) *is-server-t*))

;; desktop environment
(if (getenv "WMEmacs")
    (setq linux-desktop-env (getenv "WMEmacs"))
  (setq linux-desktop-env "i3"))

;; check owner for loading highly specialized config
(defconst *is-zyue*
  (member (user-login-name)
          '("oracleyue" "zyue" "zuogong.yue" "zuogong")))

;; fixed-width or variable-width fonts
(defconst *use-sans-orgmode* nil)

;; completion system
(defconst *ac-system* 'ivy)   ; ivy, vertico

;; auto-completion engine
(defconst *ac-engine* 'capf)  ; nil: capf, company, corfu

;; hydra supports
(defconst *use-hydra* t)

;; icons support
(defconst *icons-type* "nerd-icons")
;; /nerd-icons/ supports: dashboard, ivy, dired, ibuffer, treemacs
;; run "M-x nerd-icons-install fonts" or install nerd fonts in your os
;; (defconst *icons-type* "all-the-icons")
;; /all-the-icons/ supports: ivy, dired, company, treemacs
;; run "M-x all-the-icons-install-fonts"

;; Lsp for code intelligence
(if *is-server-m*
    (defconst *lsp-client* nil)
  (defconst *lsp-client* 'eglot))  ;; lsp-mode, lsp-bridge, eglot


(provide 'init-const)
;; ================================================
;; init-const.el ends here
