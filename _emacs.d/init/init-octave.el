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
  :init
  ;; set up "run-octave" for matlab
  (setq inferior-octave-program "/usr/local/bin/matlab"
        inferior-octave-startup-args (quote ("-nodesktop -nosplash")))
  :config
  ;; ----------- Styles for MATLAB -----------
  (add-hook 'octave-mode-hook
            (lambda () (progn (setq comment-start "%"
                               comment-add 0
                               octave-block-offset 4)
                         (defun octave-indent-comment ()
                           "A function for `smie-indent-functions' (which see)."
                           (save-excursion
                             (back-to-indentation)
                             (cond
                              ((octave-in-string-or-comment-p) nil)
                              ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}") 0))))
                         )))

  ;; ----------- Additional supports for MATLAB -----------
  ;; enable /company-dabbrev-code/ for matlab
  (when *use-company*
    (require 'company-dabbrev-code)
    (add-to-list 'company-dabbrev-code-modes 'octave-mode))

  ;; use /citre/ for jump via universal ctags in "init-programming.el"
  ;; - most keybindings in "M-s", see "M-s C-h"
  ;; - start with touching ".tags" in root and updating by "M-s u"
  ;; - "citre-jump", "citre-jump-back" (or, xref "M-." "M-,")
  ;; - "citre-peek", "citre-ace-peek"
  ;; - completion via ctags in "completion-at-point" ("C-M-i")

  ;; add /smartparens/ supports
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
