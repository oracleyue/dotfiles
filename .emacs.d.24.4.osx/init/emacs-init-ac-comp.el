;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Usage:
;; 1. "C-s", "C-r": search among candidates
;; 2. "C-o" filtering the candidates
;; 3. "C-h" show doc buffer when the popup dialog is on (partially support)
;; 4. "C-w" see the source (partially support)


;; /Company/ mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; keybindings
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)))
;(global-set-key (kbd "C-<tab>") 'company-complete)  ;; see "Integration" part
;; feature control
(setq ;company-idle-delay              nil    ;; 0 for immediate popup
      ;company-minimum-prefix-length   2
      ;company-tooltip-limit           10
      company-show-numbers            t   )

;; standarize company-backends: default
;; use "M-x company-diag" to check which backend is currently used
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         ;company-yasnippet
         )
        (company-abbrev company-dabbrev)))
;; other backends: and check the corresponding major-mode settings
;(add-to-list 'company-backends 'company-xcode)
(add-to-list 'company-backends 'company-cmake)
(add-to-list 'company-backends 'company-css)
(add-to-list 'company-backends 'company-nxml)
;; list of backends added in major-mode configs:
;;      [c/c++] company-clang, company-c-headers
;;      [python] company-jedi
;;      [R] company-ess

;; adjust colors for solarized theme
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-tooltip-search-selection ((t (:inherit isearch))))
   `(company-tooltip-search ((t (:inherit default :foreground "#d33682"))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; adding /company-quickhelp/
(company-quickhelp-mode 1)
;; starting manually
(setq company-quickhelp-delay nil)
(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))


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

(global-set-key [tab] 'tab-indent-or-complete)



;; ----------------------------------------------------------------
;; Archived codes in configurations
;; ----------------------------------------------------------------
;; add backends locally available in the major-modes
;(add-hook 'c++-mode-hook
;          (lambda ()
;            (add-to-list (make-local-variable 'company-backends)
;                         '(company-c-headers company-clang :with company-yasnippet))))