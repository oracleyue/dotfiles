;; ================================================================
;; /Company/ as the backend for code intelligent completion
;; ================================================================

;; Usage:
;; - =C-<tab>=: ~company-complete~
;; - =M-<tab>= (or =C-M-i=): ~completion-at-point~ (Emacs default)
;; - =M-/=: ~dabbrev-expand~ (Emacs default)
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
	    '(not comint-mode erc-mode message-mode help-mode gud-mode
              text-mode latex-mode org-mode markdown-mode)
	    company-backends '((company-files          ; files & directory
                            company-keywords       ; keywords
                            company-capf
                            company-dabbrev-code)
                           (company-abbrev company-dabbrev)))

  ;; enable dabbrev-code for completion in string or comments
  ;; (require 'company-dabbrev-code)
  ;; (setq company-dabbrev-code-everywhere t)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Icons and quickhelp
  (when (and emacs/>=26p *enable-company-icons*)
    (use-package company-box
      :load-path "git/company-box-20190311.1745"
      :demand
      :diminish
      :functions (my-company-box--make-line
                  my-company-box-icons--elisp)
      :commands (company-box--get-color
                 company-box--resolve-colors
                 company-box--add-icon
                 company-box--apply-color
                 company-box--make-line
                 company-box-icons--elisp)
      :hook (company-mode . company-box-mode)
      :init (setq company-box-backends-colors nil
                  company-box-show-single-candidate t
                  company-box-max-candidates 50
                  company-box-doc-delay 0.5)
      :config
      ;; Support `company-common'
      (defun my-company-box--make-line (candidate)
        (-let* (((candidate annotation len-c len-a backend) candidate)
                (color (company-box--get-color backend))
                ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                          (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
                (align-string (when annotation
                                (concat " " (and company-tooltip-align-annotations
                                                 (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                (space company-box--space)
                (icon-p company-box-enable-icon)
                (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                              (company-box--apply-color icon-string i-color)
                              (company-box--apply-color candidate-string c-color)
                              align-string
                              (company-box--apply-color annotation-string a-color)))
                (len (length line)))
          (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                           'company-box--color s-color)
                               line)
          line))
      (advice-add #'company-box--make-line :override #'my-company-box--make-line)

      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      (when (and (or *is-server* (display-graphic-p))
                 (require 'all-the-icons nil t))
        (setq company-box-scrollbar nil)
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
                (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons))))

  ) ;; END of use-package(company)

;; use /posframe/ for company
(when *use-posframe*
  (use-package company-posframe
    :if (display-graphic-p)
    :after company
    :hook (company-mode . company-posframe-mode)))

;; /Yasnippet/ A template system
(use-package yasnippet
  :demand
  :diminish (yas-minor-mode yas-global-mode)
  :config
  (yas-global-mode 1)
  (setq-default mode-require-final-newline nil))


(provide 'init-company)
;; ================================================
;; init-company.el ends here
