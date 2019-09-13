;; ================================================================
;; /Company/ as the backend for code intelligent completion
;; ================================================================

;; Install required Emacs packages
;; (setq custom/company-packages
;;       '(company
;;         company-posframe
;;         yasnippet))
;; (custom/install-packages custom/company-packages)

;; Usage:
;; - =M-/=: ~company-complete~
;; - =M-TAB= (or =C-M-i=): ~completion-at-point~ (Emacs default)
;; and
;; 1. "C-s", "C-r": search among candidates
;; 2. "C-o" filtering the candidates
;; 3. "C-h" show doc buffer when the candidate list is on
;; 4. "C-w" see the source (partially support)
;; (5.) "M-tab" calls "completion-at-point" which gives candidates in helm


;; Global settings for completion
(setq completion-ignore-case t) ;; filter candidate case-insensitive

;; /Company/ for code completion
(use-package company
  :demand
  :init
  (setq company-idle-delay                  0.2  ;; nil to disable; 0.5
	    company-tooltip-limit               10
        company-tooltip-minimum-width       40
	    company-minimum-prefix-length       3
        company-show-numbers                t
	    company-tooltip-align-annotations   t
	    company-require-match               'never
	    company-dabbrev-downcase            nil
	    company-dabbrev-ignore-case         nil
	    ;; company-dabbrev-code-other-buffers t
	    company-global-modes
	    '(not comint-mode erc-mode message-mode help-mode gud-mode
              text-mode latex-mode org-mode markdown-mode)
	    ;; company-frontends '(company-pseudo-tooltip-frontend
        ;;                     company-echo-metadata-frontend)
	    company-backends '((company-files          ; files & directory
                            company-keywords       ; keywords
                            company-capf
                            company-dabbrev-code)
                           (company-abbrev company-dabbrev)))

  :config
  (global-company-mode +1)

  (eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)))

  (global-set-key (kbd "s-/") 'company-complete-common)
  ;; default: "<M-tab>" or "M-C-i" ("<M-TAB>") for =completion-at-point=
  ;; default: "M-/" for =dabbrev-expand=

  ;; enable dabbrev-code for completion in string or comments
  (require 'company-dabbrev-code)
  (setq company-dabbrev-code-everywhere t)
  )

(when *use-posframe*
  (use-package company-posframe
    :demand
    :if (display-graphic-p)
    :after company
    :hook (company-mode . company-posframe-mode)))

;; /Yasnippet/ A template system
(use-package yasnippet
  :demand
  :config
  (yas-global-mode 1)
  (setq-default mode-require-final-newline nil))


(provide 'init-company)
;; ================================================
;; init-company.el ends here
