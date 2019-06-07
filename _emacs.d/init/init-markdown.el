;; ================================================================
;; /Markdown/: major mode for Markdown
;; ================================================================

;; Install required packages in Emacs
(setq custom/markdown-packages
      '(markdown-mode
        imenu-list
        ;; livedown
        hexo))
(custom/install-packages custom/markdown-packages)

;; Install packages in the system
;; - require "multimarkdown" or "markdown" in shell


;;
;; /markdown-mode/: major mode
;;

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; editing
  (defun zyue/variable-pitch-and-keep-whitespaces ()
    (setq-local cursor-type 'bar)
    ;; use variable-width fonts
    (variable-pitch-mode t)
    ;; restore fixed-pitch fonts
    (set-face-font 'markdown-pre-face zyue-font)
    (set-face-font 'markdown-code-face zyue-font)
    (set-face-font 'markdown-inline-code-face zyue-font)
    ;; turn off auto-fill mode
    ;; (turn-off-auto-fill)
    (setq-local fill-column *fill-column-sans*))
  (add-hook 'markdown-mode-hook 'zyue/variable-pitch-and-keep-whitespaces)

  ;; fontify code blocks
  (setq markdown-fontify-code-blocks-natively t)

  ;; face refinement
  (set-face-background 'markdown-code-face (face-background 'org-block))
  (set-face-background 'markdown-pre-face (face-background 'org-block))

  ;; outline view of headings
  ;; use /imenu-list/ in "init-dired", toggled by "C-x C-'"
  (use-package imenu-list)

  ;; configure compile commands
  (if (string-equal system-type "darwin")
      (progn
        (setq markdown-command "/usr/local/bin/multimarkdown")
        (setq markdown-open-command "/Users/oracleyue/bin/Marked2"))
    (setq markdown-command "/usr/bin/multimarkdown"))

  ;; configure markdown export styles
  (if *use-css-local*
      (setq css-default-path (expand-file-name "~/.emacs.d/templates/css/"))
    (setq css-default-path  ;; css files on github.com
        "https://rawgit.com/oracleyue/dotfiles/master/_emacs.d/templates/css/"))
  (add-hook 'markdown-mode-hook (lambda()
     (add-to-list 'markdown-css-paths (concat css-default-path "style.md.css"))
     (add-to-list 'markdown-css-paths
                  (concat css-default-path "bootstrap.min.css"))))

  );End of use-package(markdown-mode)

;; use /livedown.el/ for preview  (osx uses "Marked 2.app")
;; Note: require "node + npm" in Bash; and "~$ npm install -g livedown"
(use-package livedown
  :load-path "~/.emacs.d/git"
  :init
  (setq livedown-autostart nil) ; auto open preview when opening markdown files
  (setq livedown-open t)        ; auto open the browser window
  (setq livedown-port 1337)     ; port for livedown server
  (setq livedown-browser "safari")
  :config
  ;; use livedown to preview markdown
  (add-hook 'markdown-mode-hook (lambda()
       (define-key markdown-mode-map (kbd "C-c C-c l") 'livedown-preview)
       (define-key markdown-mode-map (kbd "C-c C-c k") 'livedown-kill))))

;; OR use /flymd/ for live preview
;; https://github.com/mola-T/flymd


;;
;; /hexo/: major mode to write blogs using .md
;;
(use-package hexo
  :ensure t
  :config
  (defun blog ()
    (interactive)
    (hexo "~/Public/Dropbox/oracleyue/oracleyue.github.io")))



(provide 'init-markdown)
;; ================================================
;; init-markdown.el ends here
