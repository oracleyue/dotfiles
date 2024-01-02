;; ================================================================
;; Settings to Writing and Reding Blogs
;; ================================================================
;; Last modified on 11 Mar 2021


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
  (setq easy-hugo-org-header nil))

;; ----------------------------------------------
;; /deft/: Organise and browse notes
;; ----------------------------------------------
(use-package deft
  :commands (deft)
  :bind (("C-x f" . deft-find-file))
  :config
  (setq deft-extensions '("org" "md")
        deft-default-extension "org"
        deft-directory  "~/Public/Dropbox/oracleyue/Notebooks")
  (setq deft-recursive t
        deft-filter-only-filenames t))

;; ------------------------------------------------------------
;; RSS reader
;; ------------------------------------------------------------
(use-package elfeed
  :demand
  :hook ((elfeed-mode . visual-line-mode)
         (elfeed-show-mode . (lambda () (setq-local line-spacing 0.1))))
  :config
  (setq elfeed-feeds
        '(("https://www.technologyreview.com/feed/" TechReview)
          ("https://deepmind.com/blog/basic/" AIBlog)
          ("https://feeds.feedburner.com/blogspot/gJZg" AIBlog)
          ("https://blogs.microsoft.com/ai/feed/" AIBlog)
          ("https://quertle.com/feed/" BiomedAI)
          ))
  ;; add vi-style scrolling
  (define-key elfeed-show-mode-map "k" (elfeed-expose #'scroll-down 20))
  (define-key elfeed-show-mode-map "j" (elfeed-expose #'scroll-up 20))
  (define-key elfeed-show-mode-map "h" 'elfeed-kill-buffer)
  (define-key elfeed-search-mode-map "k" (elfeed-expose #'previous-line))
  (define-key elfeed-search-mode-map "j" (elfeed-expose #'next-line))
  (define-key elfeed-search-mode-map "l" 'elfeed-search-show-entry)
  )
;; Database is cached in "~/.elfeed/"
;; Init or update list:
;; - M-x elfeed
;; - g:   refresh view of the feed listing
;; - G:   fetch feed updates from the servers
;; - s:   update the search filter (see tags)
;; - c:   clear the search filter
;; Interacting with entries:
;; - RET: view selected entry in a buffer
;; - b:   open selected entries in your browser (browse-url)
;; - y:   copy selected entries URL to the clipboard
;; - r:   mark selected entries as read
;; - u:   mark selected entries as unread
;; - +:   add a specific tag to selected entries
;; - -:   remove a specific tag from selected entries


(provide 'init-blog)
;; ================================================
;; init-blog.el ends here
