;; ================================================================
;; Select or Enable Features
;; ================================================================


;; select tree window manager
(defconst *tree-manager* "neotree")

;; use golden-ratio mode
(if *is-server-main*
    (defconst *use-golden-ratio* nil)
  (defconst *use-golden-ratio* nil))

;; use css locally or in github
(defconst *use-css-local* t)

;; completion system
(if *is-mac*
    (if *is-server-main*
        (defconst *use-helm* nil)
      (defconst *use-helm* nil))
  (defconst *use-helm* nil))

;; integrate TAB for yasnippet, indent and company completion
(defconst *integrate-TAB* nil)

;; code intelligence
(cond (*is-mac*
       (if *is-server-ac*
           (defconst *use-company* nil) ;; use auto-complete
         (defconst *use-company* t)))
      (*is-linux*
       (defconst *use-company* t)))
(defconst *use-lsp* nil)  ;; use LSP as a unified codeIntel

;; programming

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
