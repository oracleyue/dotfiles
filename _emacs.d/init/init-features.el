;; ================================================================
;; Select or Enable Features
;; ================================================================


;; select tree window manager
(defconst *tree-manager* "neotree")
;; (defconst *tree-manager* "direx") ;; enable /direx-jedi/ in python

;; completion system
(if *is-mac*
    (if (not (daemonp))
        (defconst *use-helm* nil)
      (defconst *use-helm* t))
  (defconst *use-helm* nil))

;; code completion engine
(cond (*is-mac*
       (if *is-server-ac*
           (defconst *use-company* nil) ;; use auto-complete
         (defconst *use-company* t)))
      (*is-linux*
       (defconst *use-company* t)))

;; programming
;; semantics
(if *is-server-main*
    (defconst *enable-semantics* nil) ;; /helm-sematic-or-imenu/, /stickyfunc/
  (defconst *enable-semantics* t))

;; c/c++
(defconst *enable-gg-cpp-style* nil)  ;; /google-c-style/
(defconst *enable-rtags* nil)         ;; /rtags/
(defconst *cc-engine* "modern")       ;; "native": company-clang
                                      ;; "modern": irony + (rtags)
(defconst *enable-function-args* nil) ;; /function-args/ (require semantics)



(provide 'init-features)
;; ================================================
;; init-features.el ends here
