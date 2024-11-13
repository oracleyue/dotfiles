;; ===============================================================
;; Ivy - a generic completion mechanism for Emacs
;; ===============================================================
;; Last modified on 31 Mar 2018

;; ---------------------------------------------
;; /Ivy + Counsel + Swiper/: by abo-abo
;; ---------------------------------------------
(use-package counsel
  :demand
  :diminish (ivy-mode counsel-mode)
  :hook ((after-init . ivy-mode)
         (ivy-mode   . counsel-mode))
  :bind (([remap switch-to-buffer] . #'ivy-switch-buffer)
         ("C-c C-r"  . ivy-resume)
         :map ivy-minibuffer-map
         ("C-SPC"    . ivy-mark))  ;; use M-o to call action
  ;; counsel
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . counsel-find-library)
         ([remap imenu]                    . counsel-imenu)
         ([remap dired]                    . counsel-dired)
         ([remap recentf-open-files]       . counsel-recentf)
         ([remap insert-char]              . counsel-unicode-char)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap org-capture]              . counsel-org-capture)
         ;; minibuffer history
         ;; "C-r" ivy-reverse-i-search acts as counsel-minibuffer-history in counsel-mode
         ;; enchance built-in functions
         ("M-g U"   . counsel-unicode-char)
         ("M-g C"   . counsel-colors-web)
         ;; kill-ring
         ("M-y"     . counsel-yank-pop)
         ;; mark-ring
         ("M-g m"   . counsel-mark-ring)
         ;; register
         ("M-g r"   . counsel-register)
         ;; bookmark (Emacs default; =C-x r b= to create bookmark)
         ("M-g b"   . counsel-bookmark)
         ;; recent files
         ("M-g h"   . counsel-recentf)  ;; or "M-g H" for "counsel-recent-directory" (user-defined)
         ;; code overview
         ("M-g i"   . counsel-semantic-or-imenu))
  ;; swiper
  :bind (("C-s"     . swiper)           ;; swiper, swiper-isearch
         ("C-r"     . swiper-backward)
         ("s-f"     . swiper-isearch)
         ("M-g s"   . counsel-grep)     ;; using rg
         ([remap swiper]          . counsel-grep-or-swiper) ;; for large files
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ;; all buffers
         ("C-S-s"   . swiper-all)
         ;; grep files recursively in the folder
         ("M-g a"   . counsel-rg)       ;; counsel-ag, counsel-ack, counsel-rg
         ;; git project
         ("C-x g"   . counsel-git)
         ("C-x s"   . counsel-git-grep)
         ;; system-wide files
         ("M-g f"   . counsel-fzf)      ;; find
         ("M-g M-l" . counsel-locate)
         :map swiper-map
         ("M-s"     . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-s"     . swiper-isearch-toggle))

  :init
  (setq enable-recursive-minibuffers t)

  ;; disable complete-symbol showing popup window at point; use minibuffer
  ;; (setq ivy-display-functions-alist nil)

  (setq ivy-wrap   t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function         #'ivy-format-function-line
        ivy-use-virtual-buffers     t ;; add recent files to ivy-switch-buffer
        ivy-use-selectable-prompt   t)  ;; make inputs selectable

  :config
  ;; auto prepend symbols in ivy commands (always set nil by "ivy-prescient")
  ;; (setq ivy-initial-inputs-alist nil)  ; nothing

  ;; number of items in completion list
  (push '(counsel-yank-pop . 15) ivy-height-alist)

  ;; more file finding
  (setq counsel-find-file-at-point t
        counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)\\|\\(.DS_Store\\)")

  ;; use the faster search tool: ripgrep ("rg")
  (when (executable-find "rg")
    (setq counsel-grep-base-command
          "rg -S --no-heading --line-number --color never %s %s")
    (when (and *is-mac* (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))
  ) ;; End of Ivy

;; ---------------------------------------------
;; Hydra support for Ivy
;; ---------------------------------------------
(use-package ivy-hydra
  :commands ivy-hydra-read-action
  :init (setq ivy-read-action-function #'ivy-hydra-read-action))

;; ---------------------------------------------
;; Better sorting for Ivy candidates
;; ---------------------------------------------
(use-package prescient)
(use-package ivy-prescient
  :demand
  :init
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch swiper-all ivy-switch-buffer
               lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
               counsel-grep counsel-git-grep counsel-rg counsel-ag
               counsel-ack counsel-pt counsel-imenu
               counsel-org-capture counsel-yank-pop
               counsel-recentf counsel-buffer-or-recentf
               ivy-reverse-i-search counsel-recent-directory))
  :config
  (ivy-prescient-mode 1))

;; ---------------------------------------------
;; Support pinyin for Chinese in Ivy
;; ---------------------------------------------
;; Input prefix '!' to match pinyin
;; For example: 你好 can be match by type !nh
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package pinyinlib
  :demand
  :commands pinyinlib-build-regexp-string
  :init
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order)))

  (setq ivy-re-builders-alist
        (append '((counsel-rg        . re-builder-pinyin)
                  (counsel-ag        . re-builder-pinyin)
                  (swiper            . re-builder-pinyin)
                  (swiper-isearch    . re-builder-pinyin)
                  (swiper-all        . re-builder-pinyin)
                  (counsel-find-file . re-builder-pinyin))
         ivy-re-builders-alist))

  (defun my-pinyinlib-build-regexp-string (str)
    (progn (cond ((equal str ".*") ".*")
                 (t (pinyinlib-build-regexp-string str t)))))

  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ") ".*")
          ((equal str "") nil)
          (t str)))

  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str)) nil)
          ((equal (substring str 0 1) "!")
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper
                                          (split-string (replace-regexp-in-string "!" "" str) ""))) ""))
          nil)))
;; To remove `pinyin' match, uncomment the following:
;; (defun pinyin-to-utf8 (str) nil)

;; ---------------------------------------------
;; Ivy integration for projectile
;; ---------------------------------------------
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input
              '(ivy-thing-at-point)))

;; ---------------------------------------------
;; Ivy integration for yasnippet
;; ---------------------------------------------
(use-package ivy-yasnippet
  :bind ("M-g y" . ivy-yasnippet))

;; ---------------------------------------------
;; Ivy integration for selecting xref candidates
;; ---------------------------------------------
(use-package ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; ---------------------------------------------
;; /Ivy-rich /: all-the-icons for Ivy interface
;; ---------------------------------------------
;; enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (icons-displayable-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

;; more friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

;; ---------------------------------------------
;; Use posframe for Ivy
;; ---------------------------------------------
(use-package ivy-posframe
  :demand
  :diminish
  :init
  (setq ivy-posframe-height 18
        ivy-posframe-parameters '((min-width   . 75) (min-height   . 15)
                                  (left-fringe . 10) (right-fringe . 10))
        ivy-posframe-border-width 0)
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper-isearch           . ivy-display-function-fallback)
          (swiper                   . ivy-display-function-fallback)
          (ivy-completion-in-region . ivy-posframe-display-at-point)
          (t                        . ivy-posframe-display)))
  (ivy-posframe-mode 1))

;; ---------------------------------------------
;; User-Extension: open recent directories
;; ---------------------------------------------
;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
;; https://emacs-china.org/t/topic/5948/3?u=et2010
(defvar counsel-recent-dir-selected nil)
(defvar counsel-recent-dir-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "TAB") 'counsel-recent-dir-find-file)
    (define-key map  [(tab)] 'counsel-recent-dir-find-file)
    map))

(defun counsel-recent-dir-find-file()
  (interactive)
  (ivy-exit-with-action
   (lambda (c)
     (setq counsel-recent-dir-selected c)
     (run-at-time 0.05 nil
                  (lambda()
                    (let ((default-directory counsel-recent-dir-selected))
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
    (ivy-read "directories: " collection
              :keymap counsel-recent-dir-map
              :caller 'counsel-recent-directory
              :action (lambda (x) (if (fboundp 'ranger) (ranger x) (dired x))))))

(global-set-key (kbd "M-g H") 'counsel-recent-directory)


(provide 'init-ivy)
;; ================================================
;; init-ivy.el ends here
