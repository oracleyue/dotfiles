(defhydra hydra-paredit-cursor (:hint nil)
  "
^<< up >>^              ^<< backword^           ^forward >>^            ^<< down >>^
^^^^^^^^----------------------------------------------------------------------------------------
_u_: b-w-up |(a |(a*))  _b_: b-w  ) |(a) |(b)*  _f_: f-w  *(a)| (b)| (  _a_: b-w-down ((|a) b|)*
_e_: up     (a (b*)|)|  _p_: prev )| (a)| (b)*  _n_: next *(a) |(b) |(  _d_: down     *(|a (|b))
"
  ("b" paredit-backward)
  ("f" paredit-forward)
  ("p" paredit-previous)
  ("n" paredit-next)
  ("a" paredit-backward-down)
  ("d" paredit-forward-down)
  ("u" paredit-backward-up)
  ("e" paredit-forward-up))

(defhydra hydra-paredit-bracket (:hint nil)
  "\n_[_: <--(    _{_: (-->    _}_: <--)    _]_: )-->\n"

  ("[" paredit-backward-slurp-sexp)
  ("{" paredit-backward-barf-sexp)
  ("]" paredit-forward-slurp-sexp)
  ("}" paredit-forward-barf-sexp))

;; 仅显示 hydra，不执行实质操作
(dotemacs-leader/set-key
 "ss" 'hydra-paredit-cursor/body)

;; 以下快捷键除了执行绑定动作，同时会显示 hydra
(let ((maps
       (if (boundp 'evil-mode)
           (list evil-normal-state-map evil-insert-state-map)
         (list global-map)))
      (defs
        `(;; 光标快捷键（经典风格）
          (hydra-paredit-cursor/body  . ("C-M-a" . paredit-backward-down))
          (hydra-paredit-cursor/body  . ("C-M-b" . paredit-backward))
          (hydra-paredit-cursor/body  . ("C-M-d" . paredit-forward-down))
          (hydra-paredit-cursor/body  . ("C-M-e" . paredit-forward-up))
          (hydra-paredit-cursor/body  . ("C-M-f" . paredit-forward))
          (hydra-paredit-cursor/body  . ("C-M-n" . paredit-next))
          (hydra-paredit-cursor/body  . ("C-M-p" . paredit-previous))
          (hydra-paredit-cursor/body  . ("C-M-u" . paredit-backward-up))
          ;; 光标快捷键（leader key 形式）
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s a") . paredit-backward-down))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s b") . paredit-backward))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s d") . paredit-forward-down))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s e") . paredit-forward-up))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s f") . paredit-forward))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s n") . paredit-next))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s p") . paredit-previous))
          (hydra-paredit-cursor/body  . (,(concat dotemacs-leader/leader " s u") . paredit-backward-up))
          ;; 括号快捷键
          (hydra-paredit-bracket/body . (,(concat dotemacs-leader/leader " s [") . paredit-backward-slurp-sexp))
          (hydra-paredit-bracket/body . (,(concat dotemacs-leader/leader " s {") . paredit-backward-barf-sexp))
          (hydra-paredit-bracket/body . (,(concat dotemacs-leader/leader " s ]") . paredit-forward-slurp-sexp))
          (hydra-paredit-bracket/body . (,(concat dotemacs-leader/leader " s }") . paredit-forward-barf-sexp))
          )))
  (mapcar (lambda(map)
            (mapcar (lambda (def)
                      (let ((hydra-body (car def))
                            (key-fn (cdr def)))
                        (define-key map (kbd (car key-fn))
                          `(lambda (&optional count)
                             (interactive "P")
                             (apply ',(cdr key-fn) count)
                             (when (not count)
                               (,hydra-body))))
                        ))
                    defs))
          maps)
  )