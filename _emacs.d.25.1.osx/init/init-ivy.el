;; ===============================================================
;; Ivy    : a generic completion mechanism for Emacs.
;; Counsel: Ivy-enhanced versions of common Emacs commands.
;; Swiper : an Ivy-enhanced alternative to isearch.
;; ===============================================================


;; /Ivy/ mode
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; /Counsel/ mode
(use-package counsel
  :ensure t
  :bind
  ("C-c g"   . counsel-git)
  ("C-c j"   . counsel-git-grep)
  ("C-c k"   . counsel-ag)
  ("C-x l"   . counsel-locate)
  :config
  (define-key minibuffer-local-map (kbd "C-r")
    'counsel-minibuffer-history))

;; /Swiper/ mode
(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))



(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
