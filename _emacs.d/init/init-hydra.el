;; ===============================================================
;; Hydra - a system to simply your keybindings
;; ===============================================================
;; Last modified on 28 Feb 2021

(use-package hydra)
(use-package ivy-hydra)

;; ----------------------------------------------------------------
;; /ace-window/ with hydra: better window management
;; ----------------------------------------------------------------
(use-package ace-window
  :bind ("M-j" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame))

(use-package transpose-frame)

;; integrate with hydra
(global-set-key (kbd "M-o") 'hydra-window/body)
(defhydra hydra-window ()
  "
Movement^^      ^Split^         ^Switch^        ^Resize^
----------------------------------------------------------------
_h_ ←           _v_ertical      _b_uffer        _H_ horzontal↓
_j_ ↓           _x_ horizontal  _f_ind files    _J_ vertical ↓
_k_ ↑           _B_anlance      _a_ce           _K_ vertical ↑
_l_ →           _T_ranspose     _s_wap          _L_ horzontal↑
_SPC_ cancel    _d_elete        _D_lt Other     _O_nly this
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" shrink-window-horizontally)
  ("J" shrink-window)
  ("K" enlarge-window)
  ("L" enlarge-window-horizontally)

  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)

  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("O" delete-other-windows)
  ("B" balance-windows)
  ("T" transpose-frame)
  ("SPC" nil))


(provide 'init-hydra)
;; ================================================
;; init-hydra.el ends here
