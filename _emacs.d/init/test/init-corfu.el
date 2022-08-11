(use-package corfu
  :init
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-quit-at-boundary t      ;; Never quit at completion boundary
        corfu-quit-no-match t)        ;; Never quit, even if there is no match
  (setq corfu-preview-current nil)    ;; Disable current candidate preview
  (setq corfu-min-width 40
        corfu-max-width 80)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (global-corfu-mode))

(use-package corfu-doc
  :defer t
  :init
  ;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "s-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "s-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "s-n") #'corfu-doc-scroll-up)   ;; corfu-previous
  )

;; use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))
