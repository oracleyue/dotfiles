;; ================================================================
;; Select or Enable Features
;; ================================================================


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
(defconst *use-posframe* t)

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
