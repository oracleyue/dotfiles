;; ----------------------------------------------------------------
;; Managing Ebooks in Calibre
;; ----------------------------------------------------------------
;; Last modified on 05 Jun 2020

(use-package calibredb
  ;; :quelpa (calibredb :fetcher github
  ;;                    :repo "chenyanming/calibredb.el"
  ;;                    :branch "develop")
  :defer t
  :commands (calibredb calibredb-find-counsel calibredb-list)
  :custom ((sql-sqlite-program "/usr/bin/sqlite3")
           (calibredb-root-dir "~/Books/Calibre_Library")
           (calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
           (calibredb-program "/Applications/calibre.app/Contents/MacOS/calibredb")
           (calibredb-library-alist '(("~/Books/Calibre_Library")
                                      ("~/Books/Calibre_Thinkpad"))))
  :config
  ;; hide comment in *calibre-search* list
  (setq calibredb-comment-width 0)

  ;; keybindings for book list and search
  (defvar calibredb-show-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "?" #'calibredb-entry-dispatch)
      (define-key map "o" #'calibredb-find-file)
      (define-key map "O" #'calibredb-find-file-other-frame)
      (define-key map "v" #'calibredb-open-file-with-default-tool)
      (define-key map "s" #'calibredb-set-metadata-dispatch)
      (define-key map "e" #'calibredb-export-dispatch)
      (define-key map "q" #'calibredb-entry-quit)
      (define-key map "\M-t" #'calibredb-set-metadata--tags)
      (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
      (define-key map "\M-A" #'calibredb-set-metadata--authors)
      (define-key map "\M-T" #'calibredb-set-metadata--title)
      (define-key map "\M-c" #'calibredb-set-metadata--comments)
      map)
    "Keymap for `calibredb-show-mode'.")
  (defvar calibredb-search-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-3] #'calibredb-search-mouse)
      (define-key map (kbd "<RET>") #'calibredb-search-ret)
      (define-key map "?" #'calibredb-dispatch)
      (define-key map "a" #'calibredb-add)
      (define-key map "A" #'calibredb-add-dir)
      (define-key map "c" #'calibredb-clone)
      (define-key map "d" #'calibredb-remove)
      (define-key map "l" #'calibredb-library-list)
      (define-key map "n" #'calibredb-library-next)
      (define-key map "p" #'calibredb-library-previous)
      (define-key map "s" #'calibredb-set-metadata-dispatch)
      (define-key map "S" #'calibredb-switch-library)
      (define-key map "o" #'calibredb-find-file)
      (define-key map "O" #'calibredb-find-file-other-frame)
      (define-key map "v" #'calibredb-open-file-with-default-tool)
      (define-key map "e" #'calibredb-export-dispatch)
      (define-key map "r" #'calibredb-search-refresh-or-resume)
      (define-key map "q" #'calibredb-search-quit)
      (define-key map "m" #'calibredb-mark-and-forward)
      (define-key map "u" #'calibredb-unmark-and-forward)
      (define-key map "U" #'calibredb-unmark-and-backward)
      (define-key map "j" #'calibredb-show-next-entry)
      (define-key map "k" #'calibredb-show-previous-entry)
      (define-key map "/" #'calibredb-search-live-filter)
      (define-key map "\M-t" #'calibredb-set-metadata--tags)
      (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
      (define-key map "\M-A" #'calibredb-set-metadata--authors)
      (define-key map "\M-T" #'calibredb-set-metadata--title)
      (define-key map "\M-c" #'calibredb-set-metadata--comments)
      map)
    "Keymap for `calibredb-search-mode'.")
  )


(provide 'init-calibre)
;; ================================================
;; init-calibre.el ends here
