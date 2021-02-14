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
  ;; new post
  (defun zyue/hugo-newpost (slug title tags categories)
    (interactive "sSlug:
sTitle:
sTags:
sCategories: ")
    (easy-hugo-with-env
     (let* ((now (current-time))
		    (basename (concat (format-time-string "%Y-%m-%d-" now)
							  slug easy-hugo-default-ext))
		    (postdir (expand-file-name easy-hugo-postdir easy-hugo-basedir))
		    (filename (expand-file-name basename postdir)))
	   (when (file-exists-p filename)
         (error "%s already exists!" filename))
	   (find-file filename)
	   (insert
	    (format "#+TITLE: %s
#+DATE: %s
#+TAGS[]: %s
#+CATEGORIES[]: %s

" title (zyue/iso-8601-date-string) tags categories))
	   (goto-char (point-max))
	   (save-buffer))))
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
