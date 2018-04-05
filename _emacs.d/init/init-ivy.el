;; ===============================================================
;; Ivy    : a generic completion mechanism for Emacs.
;; Counsel: Ivy-enhanced versions of common Emacs commands.
;; Swiper : an Ivy-enhanced alternative to isearch.
;; ===============================================================
;; Last modified on 31 Mar 2018


;; Install required Emacs packages
(setq custom/ivy-ext-packages
      '(ivy
        counsel
        swiper
        counsel-projectile))
(custom/install-packages custom/ivy-ext-packages)


;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------

;; Configurations
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Keybindings

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> k") 'counsel-descbinds)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Ivy-based interface to editing and programs
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-SPC") 'counsel-mark-ring)
(global-set-key (kbd "C-c i") 'counsel-semantic-or-imenu)

;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)

;; Minibuffer keybindings
;; go to Info Page (=C-h i=) of Ivy to see the manual
(define-key minibuffer-local-map (kbd "C-r")
  'counsel-minibuffer-history)
(require 'counsel)
(define-key counsel-find-file-map (kbd "C-o")
  'counsel-recentf)
;;  =C-M-j=: exits with the current input instead of candidates
;;  =M-i=: insert the current candidate into the minibuffer
;;  =M-o=: presents valid actions
;;  =C-j=: start a new completion; otherwise, same as =RET=
;;  =TAB=: attempts partial completion; =TAB TAB= same as =C-j=
;;  =M-n=, =M-p=: cycles through the Ivy command history
;;  =S-SPC=: deletes the current input and rests the list


;; ---------------------------------------------
;; /Counsel-Projectile/: Ivy for Projectile
;; ---------------------------------------------

;; Configurations
(counsel-projectile-mode)

;; Keybindings
;; go to =./readme= to see more complete manual
;;  =C-c p p=: switch project
;;  =C-c p f=: jump to a project file
;;  =C-c p d=: jump to a project directory
;;  =C-c p b=: jump to a project buffer
;;  =C-c p s g=: search project with grep
;;  =C-c p s s=: serach project with ag
;; new commands:
;;  =C-c p SPC=: jump to a project buffer, file, or switch project
;;  =C-c p s r=: search project with rg
;;  =C-c p O=:   Org-capture into project



(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
