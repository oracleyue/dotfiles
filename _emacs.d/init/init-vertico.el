;; ===============================================================
;; Vertico+... - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 28 Dec 2024

;; /Orderless/: better completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; /Vertico/: vertical interactive completion
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8) (right-fringe . 8)
                (min-width    . 75))))

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

;; /Consult/: consulting completing-read
(use-package consult
  :demand
  :bind (("C-x b"   . consult-buffer)                 ;; orig. switch-to-buffer
         ("C-x M-x" . consult-mode-command)

         ;; isearch
         ([remap isearch-forward] . consult-line)     ;; C-s
         ("C-S-s"   . consult-line-multi)             ;; isearch over all buffers

         ;; search
         ("M-g a"   . consult-ripgrep)    ;; alternative: consult-grep
         ("M-s s"   . consult-git-grep)

         ;; find files
         ("M-g f"   . consult-fd)         ;; alternative: consult-find
         ;; ("M-g M-l" . consult-locate)

         ;; goto line
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line

         ;; kill ring (yank)
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop
         ;; register
         ("M-g r"   . consult-register)
         ;; bookmark
         ("M-g b"   . consult-bookmark)            ;; orig. bookmark-jump
         ;; mark ring
         ("M-g m"   . consult-mark)
         ("M-g M"   . consult-global-mark)
         ;; recentf
         ([remap recentf-open-files] . consult-recent-file)
         ("M-g h"   . consult-recent-file)

         ;; programming: checker
         ("M-g e"   . consult-compile-error)
         ("M-g l"   . consult-flycheck)
         ("M-g L"   . consult-flymake)               ;; Alternative: consult-flycheck

         ;; programming: symbol listing
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)

         ;; info or help
         ([remap Info-search]  . consult-info)
         ;; ("C-c h"   . consult-history)
         ;; ("C-c m"   . consult-man)
         ;; ("C-c i"   . consult-info)

         ;; isearch integration
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string

         ;; minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)            ;; orig. next-matching-history-element
         ("M-r" . consult-history))           ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  (setq consult-preview-key nil)
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  ;; completion-at-point handled by vertico if using capf
  (when (or (eq *ac-engine* nil) (eq *ac-engine* 'capf))
    (setq completion-in-region-function #'consult-completion-in-region))
  )

(use-package consult-flyspell
  :bind ("M-g M-s" . consult-flyspell))

(use-package consult-yasnippet
  :bind ("M-g y"   . consult-yasnippet))

;; /Embark/: minibuffer actions rooted in keymaps
(use-package embark
  :bind (("C-;"   . embark-act)
         ("M-."   . embark-dwim)     ; overrides `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings) ;default: C-h b
         ("M-g H" . embark-recentf-remove))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; /Consult-citre/: support for Citre tags
;; In .emacs.d/site-lisp; From https://emacs-china.org/t/citre-ctags/17604/672
(use-package consult-citre
  :ensure nil
  :bind ("M-g t" . consult-citre)
  :config
  (consult-customize
   consult-citre
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any)))


(provide 'init-vertico)
;; ================================================
;; init-vertico.el ends here
