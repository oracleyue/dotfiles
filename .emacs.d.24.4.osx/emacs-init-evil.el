; ================================
;; Settings for /evil-mode/

;; Enable evil-leader
(require 'evil-leader)
(global-evil-leader-mode)

;; Enable evil-mode
(require 'evil)
;(evil-mode 1)

;; Enable default emacs keybindings
(define-key evil-insert-state-map "\C-e" 'org-end-of-line)
(define-key evil-normal-state-map "\C-e" 'org-end-of-line)
(define-key evil-visual-state-map "\C-e" 'org-end-of-line)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-n" 'next-line)
(define-key evil-normal-state-map "\C-n" 'next-line)
(define-key evil-visual-state-map "\C-n" 'next-line)
(define-key evil-insert-state-map "\C-p" 'previous-line)
(define-key evil-normal-state-map "\C-p" 'previous-line)
(define-key evil-visual-state-map "\C-p" 'previous-line)
(define-key evil-normal-state-map [up] 'previous-line)
(define-key evil-normal-state-map [down] 'next-line)
(define-key evil-normal-state-map [left] 'left-char)
(define-key evil-normal-state-map [right] 'right-char)
(define-key evil-insert-state-map (kbd "C-S-d") 'evil-shift-left-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-normal-state-map "\M-." nil)
(define-key evil-normal-state-map "\M-?" nil)

;; For keybindings defined by /evil-leader/			
(evil-leader/set-key 
    "cc" 'comment-region
    "cu" 'uncomment-region
)

;; Modes initialized by emacs-state
(add-hook 'matlab-shell-mode-hook 'evil-emacs-state)
(add-hook 'dired-mode-hook 'evil-emacs-state)

;; Fix for /neotree/
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
