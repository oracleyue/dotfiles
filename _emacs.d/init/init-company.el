;; ================================================================
;; /Company/ as the backend for code intelligent completion
;; ================================================================
;; Last modified on 24 Feb 2020

;; Usage:
;; - =C-<tab>=: ~company-complete~
;; - =M-<tab>= (or =C-M-i=): ~completion-at-point~ (Emacs default)
;; - =M-/=: ~dabbrev-expand~ (Emacs default)
;; and
;; 1. "C-s", "C-r": search among candidates
;; 2. "C-o" filtering the candidates
;; 3. "C-h" show doc buffer when the candidate list is on
;; 4. "C-w" see the source (partially support)

;; Global settings for completion
(setq completion-ignore-case t) ;; filter candidate case-insensitive

;; /Company/ for code completion
(use-package company
  :demand
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay                  0.2  ;; nil to disable; 0.5
        company-tooltip-limit               10
        company-tooltip-align-annotations   t
        company-tooltip-minimum-width       32
        company-minimum-prefix-length       2
        company-show-numbers                t
        company-require-match               nil
        company-dabbrev-downcase            nil
        company-dabbrev-ignore-case         nil
        ;; company-dabbrev-code-other-buffers t
        company-global-modes
        '(not comint-mode erc-mode message-mode help-mode gud-mode eshell-mode
              text-mode latex-mode org-mode markdown-mode)
        company-backends '((company-dabbrev-code company-capf :with company-yasnippet)
                           (company-keywords company-files)
                           company-dabbrev))

  ;; Enable dabbrev-code for completion in string or comments
  (require 'company-dabbrev-code)
  ;; (setq company-dabbrev-code-everywhere t)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Better icons and quickhelp: /company-box/
  (use-package company-box
    :hook (company-mode . company-box-mode)
    :init (setq company-box-scrollbar nil))
  ;; Warning: Loading for *scratch* via server has an issue, while it works well
  ;; for all other programming modes. Reloading *scratch* will load company-box.

  ) ;; END of use-package(company)


(provide 'init-company)
;; ================================================
;; init-company.el ends here
