;; ================================================================
;; Select or Enable Features
;; ================================================================

;; constants
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-terminal* (not (or (display-graphic-p) (daemonp))))
(defconst *is-app* (and (display-graphic-p) (not (daemonp))))
(defconst *is-server* (eq t (daemonp)))
(defconst *is-server-main* (string-equal "main" (daemonp)))
(defconst *is-server-coding* (string-equal "coding" (daemonp)))
(defconst *is-server-linux* (and *is-server* *is-linux*))

;; desktop environment
(if (getenv "WMEmacs")
    (setq linux-desktop-env (getenv "WMEmacs"))
  (setq linux-desktop-env "kde"))

;; select tree window manager
(defconst *tree-manager* "neotree")

;; use golden-ratio mode
(defconst *use-golden-ratio* nil)

;; use css locally or in github
(defconst *use-css-local* t)

;; completion system
(defconst *use-helm* nil)
(defconst *use-ivy* (not *use-helm*))

;; posframe
(defconst *use-posframe* nil)

;; code intelligence
(defconst *use-company* t) ;; auto-complete no longer supported
(defconst *use-lsp* t)     ;; use LSP as a unified codeIntel

;; semantics
(if *is-server-main*
    (defconst *enable-semantics* nil) ;; /helm-sematic-or-imenu/, /stickyfunc/
  (defconst *enable-semantics* t))

;; c/c++
(defconst *enable-gg-cpp-style* nil)  ;; /google-c-style/
(defconst *enable-rtags* nil)         ;; /rtags/
(defconst *enable-function-args* nil) ;; /function-args/ (require semantics)

;; python
(defconst *use-python-version* 3)

;; ECB IDE interface
(defconst *enable-ecb* nil)



(provide 'init-features)
;; ================================================
;; init-features.el ends here
