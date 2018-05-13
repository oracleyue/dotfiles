;; ================================================================
;; /Markdown/: major mode for Markdown
;; ================================================================

;; Install required packages for more functions
(setq custom/md-packages
      '(markdown-mode
        imenu-list
        hexo))
(custom/install-packages custom/md-packages)


;;
;; /markdown-mode/: major mode
;;

;; require "multimarkdown" or "markdown" in Bash
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; editing environment config
(defun y:variable-pitch-and-keep-whitespaces ()
  (setq-local cursor-type 'bar)
  ;; use variable-width fonts
  (variable-pitch-mode t)
  ;; turn off auto-fill mode
  (turn-off-auto-fill)
  (setq-local fill-column 85)
  ;; diable ~delete-trailing-whitespaces~ in markdown mode
  ;; (delete 'delete-trailing-whitespace write-file-hooks)
  )
(add-hook 'markdown-mode-hook 'y:variable-pitch-and-keep-whitespaces)

;; fontify code blocks
(setq markdown-fontify-code-blocks-natively t)

;; outline view of headings
;; use /imenu-list/ in "init-dired", toggled by "C-x C-'"

;; configure compile commands
(if (string-equal system-type "darwin")
    (progn
      (setq markdown-command "/usr/local/bin/multimarkdown")
      (setq markdown-open-command "/Users/oracleyue/bin/marked"))
  (setq markdown-command "/usr/bin/multimarkdown"))

;; configure markdown export styles
(if *use-css-local*
    (setq css-default-path (expand-file-name "~/.emacs.d/templates/css/"))
  (setq css-default-path  ;; css files on github.com
        "https://rawgit.com/oracleyue/dotfiles/master/_emacs.d/templates/css/"))
(add-hook 'markdown-mode-hook (lambda()
   (add-to-list 'markdown-css-paths (concat css-default-path "style.md.css"))
   (add-to-list 'markdown-css-paths (concat css-default-path "bootstrap.min.css"))))

;; Preview using /livedown.el/  (osx uses "Marked 2.app")
;; Note: require "node + npm" in Bash; and "~$ npm install -g livedown"
;; (require 'livedown)
;; (setq livedown:autostart nil) ; automatically open preview when opening markdown files
;; (setq livedown:open t)        ; automatically open the browser window
;; (setq livedown:port 1337)     ; port for livedown server
;; ;; use livedown to preview markdown
;; (add-hook 'markdown-mode-hook (lambda()
;;           (define-key markdown-mode-map (kbd "C-c C-c C-p") 'livedown:preview)))


;;
;; /hexo/: major mode to write blogs using .md
;;
(defun blog ()
  (interactive)
  (hexo "~/Public/Dropbox/oracleyue/oracleyue.github.io"))



(provide 'init-markdown)
;; ================================================
;; init-markdown.el ends here
