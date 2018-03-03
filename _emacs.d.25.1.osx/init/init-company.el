;; ================================================================
;; /Company/ as the backend for intelligent completion
;; ================================================================

;; Install required packages for more functions
(setq custom/company-packages
      '(company
        company-c-headers
        company-jedi
        pos-tip
        company-quickhelp))
(custom/install-packages custom/company-packages)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Usage:
;; 1. "C-s", "C-r": search among candidates
;; 2. "C-o" filtering the candidates
;; 3. "C-h" show doc buffer when the candidate list is on
;; 3b."M-h" show doc in popup dialog
;; 4. "C-w" see the source (partially support)
;; (5.) "M-tab" calls "completion-at-point" which gives candidates in helm

;; /Company/ mode: code completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)))
;(global-set-key (kbd "C-<tab>") 'company-complete)  ;; see "Integration" part

;; feature control
(setq company-idle-delay              1000000  ; 0 immediate
      company-minimum-prefix-length   2
      company-tooltip-limit           10
      company-show-numbers            t  )

;; standard company-backends: default
;; use "M-x company-diag" to check which backend is currently used
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-dabbrev-code)
        (company-abbrev company-dabbrev)))

;; other backends: and check the corresponding major-mode settings
;(add-to-list 'company-backends 'company-xcode)
;(add-to-list 'company-backends 'company-css)
;(add-to-list 'company-backends 'company-nxml)

;; enable dabbrev-code for completion in string or comments
(require 'company-dabbrev-code)
(setq company-dabbrev-code-everywhere t)

;; adjust faces for company-mode (Note: modify the corresponding theme file!)
;; use "list-faces-display" to show the list of faces

;; showing quick-help doc
;; "M-x company-show-doc-buffer": default "f1" or "C-h", when the candidate list is on.

;; use /company-quickhelp/
;; notes: random positioning is fixed by maximizing window and adding lines below
;(company-quickhelp-mode 1)
;; starting manually
(setq company-quickhelp-delay nil)
(eval-after-load 'company     ;; "M-h" for company-quickhelp
 '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;; disable company-mode under certain major modes
(defun y:disable-company-mode ()
  (company-mode -1)
  (company-quickhelp-mode -1))
(add-hook 'LaTeX-mode-hook 'y:disable-company-mode)
(add-hook 'org-mode-hook 'y:disable-company-mode)


;; /Yasnippet/ A template system
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)
(setq-default mode-require-final-newline nil)
;; integrate yas to company-mode complete list
(global-set-key (kbd "C-c y") 'company-yasnippet)


;; Integration of "indent" + /company/ + /yas/
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
            (company-complete-common)
          (indent-for-tab-command)))))

(if (display-graphic-p)
    (global-set-key (kbd "<tab>") 'tab-indent-or-complete)
  ;; <tab> is not available in terminal, "TAB" is used instead
  (global-set-key (kbd "TAB") 'tab-indent-or-complete))



(provide 'init-company)
;; ================================================
;; init-company.el ends here
