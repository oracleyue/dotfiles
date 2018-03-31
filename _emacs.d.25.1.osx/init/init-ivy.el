;; ===============================================================
;; Ivy    : a generic completion mechanism for Emacs.
;; Counsel: Ivy-enhanced versions of common Emacs commands.
;; Swiper : an Ivy-enhanced alternative to isearch.
;; ===============================================================


;; Install required emacs packages
(setq custom/ivy-ext-packages
      '(ivy
        counsel
        swiper))
(custom/install-packages custom/ivy-ext-packages)


;; Configurations
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Keybindings
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c i") 'counsel-semantic-or-imenu)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r")
  'counsel-minibuffer-history)



(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
