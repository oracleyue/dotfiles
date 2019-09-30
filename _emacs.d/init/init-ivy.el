;; ===============================================================
;; Ivy - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 31 Mar 2018


;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------
(use-package ivy
  :demand
  :diminish ivy-mode
  :config
  (setq ivy-initial-inputs-alist nil
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function #'ivy-format-function-line
        ivy-use-virtual-buffers t ;; add recent files to ivy-switch-buffer
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t  ;; make inputs selectable
        )
  ;; disable popup windows for completion-at-point; use minibuffer
  (setq ivy-display-functions-alist nil)
  ;; enable Ivy
  (ivy-mode +1)
  :bind (([remap switch-to-buffer] . #'ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  )

(when *use-posframe*
  (use-package ivy-posframe
    :ensure nil
    :load-path "git/ivy-posframe"
    :after (ivy)
    :config
    (setq ivy-fixed-height-minibuffer nil
          ;; ivy-display-function #'ivy-posframe-display-at-point
          ivy-posframe-parameters
          `((min-width . 90)
            (min-height . ,ivy-height)
            (internal-border-width . 10)))
    (setq ivy-display-functions-alist nil)
    (push '(t . ivy-posframe-display-at-point) ivy-display-functions-alist)
    ;; (push '(ivy-completion-in-region . ivy-posframe-display-at-point)
    ;;       ivy-display-functions-alist)
    (push '(swiper . ivy-posframe-display-at-window-bottom-left)
          ivy-display-functions-alist)
    (ivy-posframe-enable))
  )

(use-package counsel
  :demand
  :after (ivy)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . find-library)
         ([remap imenu]                    . counsel-imenu)
         ([remap recentf-open-files]       . counsel-recentf)
         ([remap org-capture]              . counsel-org-capture)
         ([remap swiper]                   . counsel-grep-or-swiper) ;; large files
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ;; completion
         ;; ([remap completion-at-point]      . counsel-company)
         ;; kill-ring
         ("M-y"     . counsel-yank-pop)
         ;; mark-ring
         ("M-g SPC" . counsel-mark-ring)
         ;; register
         ("M-g r"   . counsel-register)
         ;; bookmark (Emacs default; =C-x r b= to create bookmark)
         ("M-g b"   . counsel-bookmark)
         ;; code overview
         ("M-g i"   . counsel-semantic-or-imenu))

  :config
  (push '(counsel-yank-pop . 10) ivy-height-alist)
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)\\|\\(.DS_Store\\)"
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-git-cmd "rg --files"
        counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
        ;; counsel-rg-base-command
        ;; "rg -zS --no-heading --line-number --color never %s ."
        ;; counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        ;; counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s"

  ;; minibuffer actions
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; ensure recentf-list loaded on startup
  (with-eval-after-load 'counsel (recentf-mode))
  ;; disable recentf-list loading via ivy-switch-buffer
  ;; (setq recentf-initialize-file-name-history nil)

  ;; fix the bug for ivy-occur in OSX
  (when *is-mac*
    (setq counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  )

(use-package swiper
  :demand
  :bind (("C-s"   . swiper)           ;; swiper-isearch
         ("s-f"   . swiper-isearch)
         ("M-g s" . counsel-grep)     ;; using rg
         ;; all buffers
         ("C-S-s" . swiper-all)
         ;; git project
         ("C-x g" . counsel-git)
         ("C-x j" . counsel-git-grep)
         ;; grep files recursively in the folder
         ("M-g a" . counsel-rg)       ;; counsel-ag, counsel-ack, counsel-rg
         ;; system-wide files
         ("M-g f" . counsel-fzf)      ;; find
         ("M-g l" . counsel-locate))) ;; locate

;; ---------------------------------------------
;; User-Extension: open recent directories
;; ---------------------------------------------
;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
;; https://emacs-china.org/t/topic/5948/3?u=et2010
(defvar counsel-recent-dir-selected "~/")

(defvar counsel-recent-dir-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "TAB") 'counsel-recent-dir-find-file)
    (define-key map  [(tab)] 'counsel-recent-dir-find-file)
    map))

(defun counsel-recent-dir-find-file()
  (interactive)
  (ivy-exit-with-action
   (lambda(c)
     (setq counsel-recent-dir-selected c)
     (run-at-time 0.05 nil
                  (lambda()
                    (let ((default-directory counsel-recent-dir-selected))
                      ;; (find-file counsel-recent-dir-selected)
                      (counsel-find-file)))))))

(defun counsel-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string
                       (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :keymap counsel-recent-dir-map
              :action (lambda (x) (if (fboundp 'ranger) (ranger x) (dired x))))))

(global-set-key (kbd "M-g h") 'counsel-recent-directory)

;; ---------------------------------------------
;; /counsel-gtags/: Ivy for gtags (GNU global)
;; ---------------------------------------------
(setenv "GTAGSLABEL" "pygments")
(setenv "GTAGSLIBPATH" (concat (getenv "HOME") "/.gtags/")) ;; if tag system libs
(use-package counsel-gtags
  :demand
  :bind (:map counsel-gtags-mode-map
              ;; basic jumps
              ("C-c g ." . counsel-gtags-dwim)
              ("C-c g ," . counsel-gtags-go-backward)
              ("M-g ."   . counsel-gtags-dwim)
              ("M-g ,"   . counsel-gtags-go-backward)
              ("C-c g t" . counsel-gtags-find-definition)
              ("C-c g r" . counsel-gtags-find-reference)
              ("C-c g s" . counsel-gtags-find-symbol)
              ;; create/update tags
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)
              ;; jump over stacks/history
              ("C-c g [" . counsel-gtags-go-backward)
              ("C-c g ]" . counsel-gtags-go-forward))
  :hook ((c-mode c++-mode python-mode matlab-mode) . counsel-gtags-mode)
  ;; :config (setq counsel-gtags-auto-update t)
  )

;; ---------------------------------------------------------------
;; /Hydra/: make Emacs bindings that stick around
;; ---------------------------------------------------------------
(use-package hydra)

;; ---------------------------------------------------------------
;; /Avy/: jump to char/words in tree-style
;; ---------------------------------------------------------------
(use-package avy
  :demand
  :bind (("C-'"     . avy-goto-char)   ;; C-:
         ("M-'"     . avy-goto-char-2) ;; C-'
         ("M-g c"   . avy-goto-char)
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ;; ("M-g e"   . avy-goto-word-0)  ;; too many candiates
         ("M-g M-r" . avy-resume))
  :config
  (avy-setup-default)
  )

;; ---------------------------------------------------------------
;; /Ivy-rich /: all-the-icons for Ivy interface
;; ---------------------------------------------------------------
(when *enable-all-the-icons*
  (use-package ivy-rich
    :defines (all-the-icons-icon-alist
              all-the-icons-dir-icon-alist
              bookmark-alist)
    :functions (all-the-icons-icon-for-file
                all-the-icons-icon-for-mode
                all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-faicon
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (if (and buffer-file-name
                              (all-the-icons-auto-mode-match?))
                         (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                       (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (concat ivy--directory candidate))
               (file (file-name-nondirectory path))
               (icon (cond
                      ((file-directory-p path)
                       (cond
                        ((and (fboundp 'tramp-tramp-file-p)
                              (tramp-tramp-file-p default-directory))
                         (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                        ((file-symlink-p path)
                         (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                        ((all-the-icons-dir-is-submodule path)
                         (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                        ((file-exists-p (format "%s/.git" path))
                         (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                        (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                             (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                      ((string-match "^/.*:$" path)
                       (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                      ((not (string-empty-p file))
                       (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display variable icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-face-icon (_candidate)
      "Display face icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display keybindings icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

    (when (display-graphic-p)
      (defun ivy-rich-bookmark-type-plus (candidate)
        (let ((filename (ivy-rich-bookmark-filename candidate)))
          (cond ((null filename)
                 (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                 (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
                ((not (file-exists-p filename))
                 (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                 (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'ivy-rich-bookmark-type-plus))
    :hook (;; (ivy-mode . ivy-rich-mode)
           (ivy-rich-mode . (lambda ()
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)

    ;; Setting tab size to 1, to insert tabs as delimiters
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (setq tab-width 1)))

    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 40))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 18 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 40))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 18 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            persp-switch-to-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-M-x
            (:columns
             ((ivy-rich-function-icon)
              (counsel-M-x-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((ivy-rich-function-icon)
              (counsel-describe-function-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
             :delimiter "\t")
            counsel-describe-variable
            (:columns
             ((ivy-rich-variable-icon)
              (counsel-describe-variable-transformer (:width 50))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))
             :delimiter "\t")
            counsel-describe-face
            (:columns
             ((ivy-rich-face-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-descbinds
            (:columns
             ((ivy-rich-keybinding-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-read-file-transformer))
             :delimiter "\t")
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-dired
            (:columns
             ((ivy-rich-file-icon)
              (ivy-read-file-transformer))
             :delimiter "\t")
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 0.8))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
             :delimiter "\t")
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 40))
              (ivy-rich-bookmark-info))
             :delimiter "\t")
            counsel-projectile-switch-project
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-file-transformer))
             :delimiter "\t")
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-dir-transformer))
             :delimiter "\t")
            treemacs-projectile
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t"))))
  :config
  (ivy-rich-mode 1))

;; ---------------------------------------------------------------
;; Ivy-based Packages
;; ---------------------------------------------------------------

;; Ivy for Dash (Mac only, provides "dash-in-ivy")
(use-package ivy-dash :load-path "git")


(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
