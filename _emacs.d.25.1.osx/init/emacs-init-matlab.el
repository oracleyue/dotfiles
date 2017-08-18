;;
;; Add "align" supports for matlab: align both "=" and "%"
;;
;; Alternatively, use "align-regexp" ("C-x M-a" defined in "emacs-init-basic.el"),
;; one may prefix "C-u" for more arguments.
(add-hook 'align-load-hook (lambda ()
   (add-to-list 'align-rules-list
                '(matlab-comment-align
                  (regexp . "\\(\\s-*\\)\\(%.*\\s-*\\)$")
                  (modes . '(matlab-mode))
                  (spacing . 4)
                  (repeat . nil)))
   (add-to-list 'align-rules-list
                '(matlab-equal-sign-align
                  (regexp . "\\(\\s-*\\)\\(=.*\\s-*\\)$")
                  (modes . '(matlab-mode))
                  (repeat . nil)))
   ))
