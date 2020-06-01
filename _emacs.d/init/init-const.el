;; ================================================================
;; Select or Enable Features
;; ================================================================

;; versions
(defconst emacs/>=26p (>= emacs-major-version 26) "Emacs is 26 or above.")
(defconst emacs/>=27p (>= emacs-major-version 27) "Emacs is 27 or above.")

;; systems and runtimes
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-app* (and (display-graphic-p) (not (daemonp))))
(defconst *is-server* (not (not (daemonp))))
(defconst *is-server-main* (string-equal "main" (daemonp)))
(defconst *is-server-coding* (string-equal "coding" (daemonp)))
(defconst *is-terminal* (not (or (display-graphic-p) (daemonp))))
(defconst *is-graphic* (or (display-graphic-p) (daemonp)))

;; desktop environment
(if (getenv "WMEmacs")
    (setq linux-desktop-env (getenv "WMEmacs"))
  (setq linux-desktop-env "kde"))

;; check owner for loading highly specialized config
(defconst *is-zyue*
   (member (user-login-name) '("oracleyue" "zyue" "zuogong.yue" "zuogong")))

;; fixed-width or variable-width fonts
(defconst *use-sans-orgmode* nil)

;; all-the-icons support for ivy, dired, company
(defconst *enable-all-the-icons* t)
(defconst *enable-company-icons* (and *enable-all-the-icons*
                                      t))
;; select tree window manager
(defconst *tree-manager* "neotree")  ;; neotree; treemacs

;; use golden-ratio mode
(defconst *use-golden-ratio* nil)

;; use css locally or in github
(defconst *use-css-local* nil)

;; completion system
(defconst *use-ivy*  t)
(defconst *use-helm* (not *use-ivy*))

;; posframe
(defconst *use-posframe* nil)

;; code intelligence
(defconst *use-company* t) ;; auto-complete no longer supported
(defconst *use-lsp* t)     ;; use LSP as a unified codeIntel
(defconst *use-mspyls* t)  ;; use Microsoft pyls

;; semantics
(defconst *enable-semantics* nil) ;; /helm-sematic-or-imenu/, /stickyfunc/

;; c/c++
(defconst *enable-gg-cpp-style* nil)  ;; /google-c-style/
(defconst *enable-rtags* nil)         ;; /rtags/
(defconst *enable-function-args* nil) ;; /function-args/ (require semantics)

;; python
(defconst *use-python-version* 3)
(defconst *use-ipython* nil)

;; ECB IDE interface
(defconst *enable-ecb* nil)


(provide 'init-const)
;; ================================================
;; init-const.el ends here
