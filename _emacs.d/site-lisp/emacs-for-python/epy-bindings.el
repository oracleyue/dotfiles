;; Notes: having been modified by oracleyue.
;; Last modified on 24 Aug 2017


;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; calc-mode more comfortable
;; (global-set-key (kbd "M-c") 'calc-dispatch)  ;; oracleyue, "M-c" conflicts

;; Ctrl+tab mapped to Alt+tab
;(define-key function-key-map [(control tab)] [?\M-\t])  ;oracleyue, commented

(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; Rope bindings
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (define-key python-mode-map "\C-ci" 'rope-auto-import)
;; 	    (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip)
;;         (define-key python-mode-map "\C-c\C-e" 'rope-show-calltip)
;;         (define-key python-mode-map "\C-ce" 'rope-show-doc)))


(provide 'epy-bindings)
