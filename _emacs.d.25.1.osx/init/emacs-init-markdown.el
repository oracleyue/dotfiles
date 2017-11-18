;; ================================================================
;; /Markdown/: major mode for Markdown
;; ================================================================

;; Note: require "multimarkdown" or "markdown" in Bash
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; use variable-width fonts
;; (add-hook 'markdown-mode-hook
;;           (lambda () (variable-pitch-mode t)))

;; configure compile commands
(if (string-equal system-type "darwin")
    (progn
      (setq markdown-command "/usr/local/bin/multimarkdown")
      (setq markdown-open-command "/Users/oracleyue/bin/marked"))
  (setq markdown-command "/usr/bin/multimarkdown"))

;; configure markdown export styles
(setq css-default-path (expand-file-name "~/.emacs.d/templates/css/"))
(setq url-boostrap-min-css
      "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css")
(add-hook 'markdown-mode-hook (lambda()
;; (add-to-list 'markdown-css-paths url-boostrap-min-css)  ;; if use bootstrap.min.css online
   (add-to-list 'markdown-css-paths (concat css-default-path "style.css"))
   (add-to-list 'markdown-css-paths (concat css-default-path "bootstrap.min.css"))))

;; Preview using /livedown.el/  (osx uses "Marked 2.app")
;; Note: require "node + npm" in Bash; and "~$ npm install -g livedown"
;(require 'livedown)
;(setq livedown:autostart nil) ; automatically open preview when opening markdown files
;(setq livedown:open t)        ; automatically open the browser window
;(setq livedown:port 1337)     ; port for livedown server
; ;; use livedown to preview markdown
;(add-hook 'markdown-mode-hook (lambda()
;          (define-key markdown-mode-map (kbd "C-c C-c C-p") 'livedown:preview)))



(provide 'emacs-init-markdown)
;; ================================================
;; emacs-init-markdown.el ends here