;; ================================================================
;; /Markdown/: major mode for Markdown
;; ================================================================

;; Install packages in the system
;; - require "multimarkdown" or "markdown" in shell


;;
;; /markdown-mode/: major mode
;;

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config

  ;; use variable-width fonts
  (defun zyue/variable-pitch-and-keep-whitespaces ()
    (setq-local cursor-type 'bar)
    ;; use variable-width fonts
    (variable-pitch-mode t)
    ;; restore fixed-pitch fonts
    (let ((font-name (face-attribute 'default :family)))
      (set-face-font 'markdown-pre-face font-name)
      (set-face-font 'markdown-code-face font-name)
      (set-face-font 'markdown-inline-code-face font-name))
    ;; fill columns
    (setq-local fill-column *fill-column-sans*))
  ;; (add-hook 'markdown-mode-hook 'zyue/variable-pitch-and-keep-whitespaces)

  ;; fontify code blocks
  (setq markdown-fontify-code-blocks-natively t)

  ;; face refinement
  (set-face-background 'markdown-code-face (face-background 'org-block))
  (set-face-background 'markdown-pre-face (face-background 'org-block))

  ;; outline view of headings
  ;; use /imenu-list/ in "init-dired", default toggled by "C-x C-'"
  (use-package imenu-list
    :bind ((:map markdown-mode-map
                ("C-c =" . imenu-list-smart-toggle))
           (:map imenu-list-major-mode-map
                 ("C-c =" . imenu-list-smart-toggle))))

  ;; configure compile commands
  (if (string-equal system-type "darwin")
      (progn
        (setq markdown-command "/usr/local/bin/multimarkdown")
        (setq markdown-open-command "/Users/zyue/bin/Marked2"))
    (setq markdown-command "/usr/bin/multimarkdown"))

  ;; configure markdown export styles
  (add-to-list 'markdown-css-paths
               (concat (expand-file-name "~/.emacs.d/templates/css/") "github.md.css"))
  ;; using "style.md.css" requires "bootstrap.min.css"

  ;; use mathjax
  (setq markdown-xhtml-header-content
        (concat "<script type=\"text/javascript\" async src=\""
                "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_HTML"
                "\"></script>"))

  ) ;End of use-package(markdown-mode)


(provide 'init-markdown)
;; ================================================
;; init-markdown.el ends here
