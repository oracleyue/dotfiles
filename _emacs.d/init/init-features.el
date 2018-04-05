;; ================================================================
;; Select or Enable Features
;; ================================================================


;; Select Tree Window Manager
;; (setq y:tree-manager "neotree")
(setq y:tree-manager "direx") ;; enable /direx-jedi/ in python

;; Completion Engine
(cond (*is-mac*
       (if *is-server-ac*
           (defconst *use-company* nil) ;; use auto-complete
         (defconst *use-company* t)))
      (*is-linux*
       (defconst *use-company* t)))

;; Features on C/C++ Programming
(if (or *is-server-main* (not (daemonp)))
    (setq y:enable-cedet-semantics nil) ;; /helm-sematic-or-imenu/, /stickyfunc/
  (setq y:enable-cedet-semantics t))
(setq y:enable-function-args nil)       ;; /function-args/ (require semantics)
(setq y:enable-google-cpp-style nil)    ;; /google-c-style/
(setq y:cc-complete-engine "irony")     ;; company-clang, irony, modern


(provide 'init-features)
;; ================================================
;; init-features.el ends here
