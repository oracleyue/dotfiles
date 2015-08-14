;; ========================================
;; Settings for tags support

;; For /built-in tags-update/, using =ctags -e=, NOT etags!
(defun oy-build-ctags ()
  (interactive)
  (message ">> building project tags ...")
  ;(let ((root "~/Workspace/"))
  (let ((root default-directory))
        (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " default-directory)))
  (oy-visit-project-tags)
  (message ">> tags built successfully!"))
(defun oy-visit-project-tags ()
  (interactive)
  (let ((tags-file (concat default-directory "TAGS")))
        (visit-tags-table tags-file)
        (message (concat "Loaded " tags-file))))
(global-set-key (kbd "C-c u") 'oy-build-ctags)
;(define-key evil-normal-state-map (kbd "C-w u") 'oy-build-ctags) ;up
;(evil-leader/set-key "up" 'oy-build-ctags)


;; For /etags-table/
(require 'etags-table)
(setq etags-table-search-up-depth 10)
(setq tags-table-list '("~/Workspace/TAGS" "~/tmp/TAGS"))


;; For /etags-select/
;;;; [SOURCE]: https://github.com/emacsmirror/etags-select
(add-to-list 'load-path "~/.emacs.d/git/etags-select")
(load "etags-select.el")
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
;; Setting key bindings to close etags-select window
(define-key etags-select-mode-map (kbd "C-g") 'etags-select-quit)
        ;; Also quit etags-select when cursor moves to another window
(define-key etags-select-mode-map (kbd "C-x o") 'etags-select-quit)
;; (define-key etags-select-mode-map (kbd "C-p") 'etags-select-previous-tag)
;; (define-key etags-select-mode-map (kbd "C-n") 'etags-select-next-tag)
;; default etags-select bindings
        ;; Return -> 'etags-select-goto-tag
        ;; M-Return -> 'etags-select-goto-tag-other-window
        ;; p -> 'etags-select-previous-tag
        ;; n -> 'etags-select-next-tag
        ;; q -> 'etags-select-quit
        ;; 0 -> (etags-select-by-tag-number "0")
        ;; 1 -> (etags-select-by-tag-number "1")
        ;; .. ..
        ;; 9 -> (etags-select-by-tag-number "9")
