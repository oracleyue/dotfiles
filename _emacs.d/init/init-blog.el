;; ================================================================
;; Settings to write blogs
;; ================================================================
;; Last modified on 07 Dec 2020


;; ------------------------------------------------------------
;; Hugo
;; ------------------------------------------------------------
(use-package easy-hugo
  :demand
  :init
  (setq easy-hugo-basedir "~/Workspace/Websites/myblog-hugo/"
        easy-hugo-url "https://oracleyue.github.io")
  (setq easy-hugo-default-ext ".org")
  :config
  ;; use default org header, if nil (default), use templates in hugo's "archetypes/"
  ;; (setq easy-hugo-org-header t)
  )


;; ------------------------------------------------------------
;; Hexo (DISABLED)
;; ------------------------------------------------------------
(use-package hexo
  :disabled
  :demand nil
  :config
  (defun zyue/hexo ()
    (interactive)
    (hexo "~/Public/Dropbox/oracleyue/oracleyue.github.io"))

  (defun zyue/hexo-ox-gfm (&optional async subtreep visible-only)
    "Automatically export the current .org to .md at the folder of
Hexo blog."
    (interactive)
    (let ((outfile (org-export-output-file-name
                    ".md" subtreep
                    "~/Public/Dropbox/oracleyue/oracleyue.github.io/source/_posts"))
          (org-export-with-toc nil))
      (org-export-to-file 'gfm outfile async subtreep visible-only)))
  )


(provide 'init-blog)
;; ================================================
;; init-blog.el ends here
