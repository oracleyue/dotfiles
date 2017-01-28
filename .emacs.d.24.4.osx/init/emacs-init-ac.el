;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; /Yasnippet/ A template system
(require 'popup)
(require 'yasnippet) ;; not yasnippet-bundle
;; to add user-defined snippet paths: (add-to-list 'yas-snippet-dirs "PATH NAME")
(yas-global-mode 1)
(setq-default mode-require-final-newline nil)
;; use popup in yasnippet
;(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
;; use ido in yasnippet popup menu
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-x-prompt yas-no-prompt))   ; note that yas-dropdown-prompt needs /dropdown-list/ to be installed!

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; For /auto-complete/
(require 'auto-complete-config)
(ac-config-default)
;; enable ac-mode for major modes
(setq ac-modes (append ac-modes '(matlab-mode makefile-gmake-mode makefile-bsdmake-mode)))
(setq ac-modes (append ac-modes '(cmake-mode)))
;; fix switching when line number jumps to one digit due to popup
(ac-linum-workaround)
;; starting type
(setq ac-expand-on-auto-complete t)    ;; new
;; different ways to start ac-complete
    ;1; default auto start complete when type 3 characters
    ;(setq ac-auto-start 3)
    ;2; explicit call to auto-complete, using trigger-key or auto-complete func
    (setq ac-auto-start nil)
    (ac-set-trigger-key "TAB")    ; trigger to start ac-complete
    ;3; certain modes enable/disable ac-auto-start
    ;(add-hook 'emacs-lisp-mode-hook (lambda () (setq ac-auto-start 4)))
    ;(add-hook 'lisp-interaction-mode-hook (lambda () (setq ac-auto-start 4)))
    ;(add-hook 'python-mode-hook (lambda () (setq ac-auto-start nil)))
    ;(add-hook 'c++-mode-hook (lambda () (setq ac-auto-start nil)))
;; set keys to control ac-complete behaviors
;(define-key ac-mode-map [(control tab)] 'auto-complete)    ; force to start ac-complete
;(define-key ac-completing-map [(meta return)] 'ac-stop)    ; force to stop ac-complete
;; setting of pop-up boxes
(setq ac-quick-help-delay 0.2)    ;; default "0.2"
;(setq ac-candidate-limit 100)    ;; new
;(setq ac-menu-height 20)    ;; new
;(setq ac-quick-help-height 20)    ;; new
;; show menu immediately...
(setq ac-auto-show-menu t)    ;; new
(setq ac-show-menu-immediately-on-auto-complete t)    ;; new
;; select candidates in ac-menu
(setq ac-use-menu-map t)
;; ;; set the width of popup menu to fix bugs
(setq ac-max-width 0.4)
;; keystokes
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
;; set dictionary
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140515.1959/dict/")
;; list for auto-complete
(set-default 'ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-filename    ;; new
                           ac-source-yasnippet
                           ac-source-words-in-same-mode-buffers))
; Notes: other settings are in different programming languages' parts. See C/C++.
;; ------------------------------------------------------------


;; Integration of /indent/ + /yas/ + /ac/ for TAB key
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (auto-complete)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)
