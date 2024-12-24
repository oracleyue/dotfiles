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
        company-minimum-prefix-length       3
        company-show-numbers                t
        company-require-match               nil
        company-dabbrev-downcase            nil
        company-dabbrev-ignore-case         nil
        ;; company-dabbrev-code-other-buffers t
        company-global-modes
        '(not comint-mode erc-mode message-mode help-mode gud-mode eshell-mode
              text-mode latex-mode org-mode markdown-mode)
        company-backends '(;; (company-files company-capf)
                           (company-files company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))

  ;; Enable dabbrev-code for completion in string or comments
  (require 'company-dabbrev-code)
  ;; (setq company-dabbrev-code-everywhere t)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Support for programming languages
  ;; /MATLAB/: check init-octave.el
  (add-to-list 'company-dabbrev-code-modes 'octave-mode)
  ;; /CMake/: check init-cmake.el
  (add-to-list 'company-dabbrev-code-modes 'cmake-mode)
  (defun zyue/company-cmake-setup ()
    (setq-local company-backends
                  (append '((company-cmake company-dabbrev-code))
                          company-backends)))
  (add-hook 'cmake-mode-hook 'zyue/company-cmake-setup)
  ;; /R/: check init-r.el
  (setq ess-use-company t)
  (add-to-list 'company-dabbrev-code-modes 'ess-mode)
  (defun zyue/add-company-backend-ess ()
    (pop company-backends)
    (setq-local company-backends
                (append '((company-R-args company-R-objects company-R-library
                                          company-dabbrev-code))
                        company-backends)))
  (add-hook 'ess-mode-hook #'zyue/add-company-backend-ess)

  ;; Better icons and quickhelp
  ;; Bug: Loading for *scratch* via server has an issue, while it works well
  ;; for all other programming modes. Reloading *scratch* will load company-box.
  (when (and emacs/>=26p (all-the-icons-displayable-p))
    (use-package company-box
      :diminish
      :defines company-box-icons-all-the-icons
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon t
                  company-box-backends-colors nil
                  company-box-scrollbar nil
                  company-box-doc-delay 0.5)
      :config
      ;; prettify icons
      (with-no-warnings
        (defun my-company-box-icons--elisp (candidate)
          (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))
        (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

      (when (all-the-icons-displayable-p)
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
                (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
                (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
                (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
              company-box-icons-alist 'company-box-icons-all-the-icons))))

  ) ;; END of use-package(company)


(provide 'init-company)
;; ================================================
;; init-company.el ends here
