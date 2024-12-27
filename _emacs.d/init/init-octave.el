;; ================================================================
;; Programming Environment for /MATLAB/
;; ================================================================
;; Last modified on 15 Jul 2021

(use-package octave
  :ensure nil
  :demand
  :mode ("\\.m$" . octave-mode)
  :bind (:map octave-mode-map
              ("C-c C-c" . align))
  :config
  ;; code completion
  ;; - use /company/: check init-company.el
  (when (eq *ac-engine* 'company)
      (add-to-list 'company-dabbrev-code-modes 'octave-mode))
  ;; - use capf: add dabbrev (check init-basics.el)
  (when (eq *ac-engine* 'capf)
    (add-hook 'octave-mode-hook
              (lambda() (add-to-list 'completion-at-point-functions
                                'dabbrev-complation-at-point))))

  ;; code completion (LSP)
  ;; use /eglot/ or /lsp-bridge/ for intelligent completion

  ;; style
  (defun zyue/octave-code-style ()
    (progn (setq comment-start "%"
                 comment-add 0
                 octave-block-offset 4)
           (defun octave-indent-comment ()
             "A function for `smie-indent-functions' (which see)."
             (save-excursion
               (back-to-indentation)
               (cond
                ((octave-in-string-or-comment-p) nil)
                ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}") 0))))))
  (add-hook 'octave-mode-hook #'zyue/octave-code-style)

  ;; ----------- Additional Supports for MATLAB -----------

  ;; Tag support: /citre/ in "init-programming.el"
  ;; - keys prefixed with "M-s"
  ;; - hydra support: "M-s SPC"
  ;; - start with adding ".tags" in root and updating by "M-s u"
  ;; - "citre-jump", "citre-jump-back" (or, xref "M-." "M-,")
  ;; - "citre-peek", "citre-ace-peek"
  ;; - completion via ctags in "completion-at-point" ("C-M-i")

  ;; /smartparens/ supports
  (with-eval-after-load "smartparens"
    (sp-with-modes 'octave-mode
      (sp-local-pair "'" "'" :unless '(sp-point-after-word-p)
                     :actions '(insert wrap autoskip navigate))))

  ;; use "align" for matlab: aligning both "=" and "%"
  ;; alternative: use "align-regexp" ("C-x M-a" in "init-edit.el"),
  (add-hook 'align-load-hook
            (lambda ()
              (add-to-list 'align-rules-list
                           '(matlab-comment-align
                             (regexp . "\\(\\s-*\\)\\(%.*\\s-*\\)$")
                             (modes . '(octave-mode))
                             (spacing . 4)
                             (repeat . nil)))
              (add-to-list 'align-rules-list
                           '(matlab-equal-sign-align
                             (regexp . "\\(\\s-*\\)\\(=.*\\s-*\\)$")
                             (modes . '(octave-mode))
                             (repeat . nil)))))
  ) ;; END of use-package

;; Fixing: unbind uncessary keys (for xref integration with Citre)
(define-key octave-mode-map (kbd "M-.") nil)


(provide 'init-octave)
;; ================================================
;; init-octave.el ends here
