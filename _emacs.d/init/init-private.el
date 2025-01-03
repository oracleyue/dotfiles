;; ================================================================
;; Private Settings
;; ================================================================
;; [Warning]: You shouldn't load this file, since it is highly specialized
;; for my own computer, such personal folders, files, etc.

;; open default folders/files on startup
(setq dropbox (expand-file-name "~/Public/Dropbox/"))
(cond
 (*is-server-m*
  (dired (expand-file-name "Academia" dropbox))
  (dired (expand-file-name "oracleyue/GTD" dropbox))
  (dired (expand-file-name "oracleyue/Notebooks/Research" dropbox)))
 (*is-server-c*
  (cd (expand-file-name "Workspace" dropbox))))

;; registers for easy file opening
(setq paper-root (concat dropbox "Academia/Journals/"))
(set-register ?a (cons 'file (concat paper-root "NLNetSamplSL_SA/main_SA.tex")))
(set-register ?b (cons 'file (concat paper-root "CvxClustNetData_24/main.tex")))
(setq code-root (concat dropbox "Workspace/"))
(set-register ?h (cons 'file (concat code-root "netCvxClust/_cvxclust.py")))
(set-register ?j (cons 'file (concat code-root "NLNetSamplSL/solvers/samplSL/jointSamplSL.m")))


(provide 'init-private)
;; ================================================
;; init-private.el ends here
