;;; elegant-light-theme.el --- A very minimal but elegant and consistent theme -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 Nicolas P. Rougier and Nicolò Zorzetto
;;; -------------------------------------------------------------------
;;; Authors: Nicolas P. Rougier and Nicolò Zorzetto
;;; Modified by oracleyue
;;; -------------------------------------------------------------------
;;; URL: https://github.com/rougier/elegant-emacs

(deftheme elegant-light
  "A simple yet elegant theme for emacs.")

(defface elegant-critical nil
  "Basic face for elegant theme."
  :group 'elegant)
(defface elegant-popout nil
  "Basic face for elegant theme."
  :group 'elegant)
(defface elegant-strong nil
  "Basic face for elegant theme."
  :group 'elegant)
(defface elegant-salient nil
  "Basic face for elegant theme."
  :group 'elegant)
(defface elegant-faded nil
  "Basic face for elegant theme."
  :group 'elegant)
(defface elegant-subtle nil
  "Basic face for elegant theme."
  :group 'elegant)

(defcustom elegant-modeline-disabled nil
  "Set nil to use headline to replace modeline."
  :type 'boolean
  :group 'elegant)

;; Apply faces
(let ((class '((class color) (min-colors 88) (background light)))
      (bg           "#ffffff")
      (fg           "#333333")
      (fg-critical  "#ffffff")
      (bg-critical  "#ff6347")
      (fg-popout    "#ffa07a")
      (fg-strong    "#333333")
      (fg-salient   "#00008b")
      (fg-faded     "#999999")
      (bg-subtle    "#f0f0f0"))
  (apply 'custom-theme-set-faces 'elegant-light
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `(;; Base
            (default          :background ,bg          :foreground ,fg)
            (elegant-critical :foreground ,fg-critical :background ,bg-critical)
            (elegant-popout   :foreground ,fg-popout)
            (elegant-strong   :foreground ,fg-strong   :weight regular)
            (elegant-salient  :foreground ,fg-salient  :weight regular)  ;; light
            (elegant-faded    :foreground ,fg-faded    :weight light)
            (elegant-subtle   :background ,bg-subtle)

            ;; Structural
            (bold              :inherit     elegant-strong  :weight      bold)
            (italic            :inherit      elegant-faded  :slant     italic)
            (underline         :inherit      elegant-faded  :underline      t)
            (bold-italic       :inherit     elegant-strong
                               :weight                bold  :slant italic)
            (region            :background       ,fg-faded)
            (highlight         :inherit     elegant-subtle)
            (hl-line           :inherit     elegant-subtle)

            ;; (fixed-pitch       :inherit      default)
            ;; (fixed-pitch-serif :inherit      default)
            ;; (variable-pitch    :inherit      default)
            (cursor            :inherit      default)

            ;; Semantic
            (shadow            :inherit      elegant-faded)
            (success           :inherit    elegant-salient)
            (warning           :inherit     elegant-popout)
            (error             :inherit   elegant-critical)

            ;; General
            (buffer-menu-buffer    :inherit    elegant-strong)
            (minibuffer-prompt     :inherit    elegant-strong)
            (link                  :inherit   elegant-salient)
            (fringe                :inherit     elegant-faded)
            (isearch               :inherit    elegant-strong)
            (isearch-fail          :inherit     elegant-faded)
            (lazy-highlight        :inherit    elegant-subtle)
            (trailing-whitespace   :inherit    elegant-subtle)
            (show-paren-match      :inherit    elegant-popout)
            (show-paren-mismatch   :inherit           default)
            (tooltip               :height               0.85)

            ;; Programming
            (font-lock-comment-face        :inherit     elegant-faded)
            (font-lock-doc-face            :inherit     elegant-faded)
            (font-lock-string-face         :inherit    elegant-popout)
            (font-lock-constant-face       :inherit   elegant-salient)
            (font-lock-warning-face        :inherit    elegant-popout)
            (font-lock-function-name-face  :inherit    elegant-strong)
            (font-lock-variable-name-face  :inherit    elegant-strong)
            (font-lock-builtin-face        :inherit   elegant-salient)
            (font-lock-type-face           :inherit   elegant-salient)
            (font-lock-keyword-face        :inherit   elegant-salient)

            ;; Company
            (company-tooltip               :inherit    elegant-subtle)
            (company-tooltip-selection     :inherit            region)

            ;; Ivy
            (ivy-posframe                  :inherit    elegant-subtle)

            ;; Documentation
            (info-menu-header   :inherit       elegant-strong)
            (info-header-node   :inherit              default)
            (Info-quoted        :inherit        elegant-faded)
            (info-title-1       :inherit       elegant-strong)
            (info-title-2       :inherit       elegant-strong)
            (info-title-3       :inherit       elegant-strong)
            (info-title-4       :inherit       elegant-strong)

            ;; Bookmarks
            (bookmark-menu-heading  :inherit   elegant-strong)
            (bookmark-menu-bookmark :inherit  elegant-salient)

            ;; Message
            (message-cited-text         :inherit        elegant-faded)
            (message-header-cc          :inherit              default)
            (message-header-name        :inherit       elegant-strong)
            (message-header-newsgroups  :inherit              default)
            (message-header-other       :inherit              default)
            (message-header-subject     :inherit      elegant-salient)
            (message-header-to          :inherit      elegant-salient)
            (message-header-xheader     :inherit              default)
            (message-mml                :inherit       elegant-popout)
            (message-separator          :inherit        elegant-faded)

            ;; Outline
            (outline-1  :inherit                   elegant-strong)
            (outline-2  :inherit                   elegant-strong)
            (outline-3  :inherit                   elegant-strong)
            (outline-4  :inherit                   elegant-strong)
            (outline-5  :inherit                   elegant-strong)
            (outline-6  :inherit                   elegant-strong)
            (outline-7  :inherit                   elegant-strong)
            (outline-8  :inherit                   elegant-strong)

            ;; Interface ("cus-edit")
            (widget-field              :inherit      elegant-subtle)
            (widget-button             :inherit      elegant-strong)
            (widget-single-line-field  :inherit      elegant-subtle)
            (custom-group-subtitle     :inherit      elegant-strong)
            (custom-group-tag          :inherit      elegant-strong)
            (custom-group-tag-1        :inherit      elegant-strong)
            (custom-comment            :inherit       elegant-faded)
            (custom-comment-tag        :inherit       elegant-faded)
            (custom-changed            :inherit     elegant-salient)
            (custom-modified           :inherit     elegant-salient)
            (custom-face-tag           :inherit      elegant-strong)
            (custom-variable-tag       :inherit             default)
            (custom-invalid            :inherit      elegant-popout)
            (custom-visibility         :inherit     elegant-salient)
            (custom-state              :inherit     elegant-salient)
            (custom-link               :inherit     elegant-salient)

            ;; Package
            (package-description        :inherit            default)
            (package-help-section-name  :inherit            default)
            (package-name               :inherit    elegant-salient)
            (package-status-avail-obso  :inherit      elegant-faded)
            (package-status-available   :inherit            default)
            (package-status-built-in    :inherit    elegant-salient)
            (package-status-dependency  :inherit    elegant-salient)
            (package-status-disabled    :inherit      elegant-faded)
            (package-status-external    :inherit            default)
            (package-status-held        :inherit            default)
            (package-status-incompat    :inherit      elegant-faded)
            (package-status-installed   :inherit    elegant-salient)
            (package-status-new         :inherit            default)
            (package-status-unsigned    :inherit            default)

            ;; Flyspell
            (flyspell-duplicate :inherit             elegant-popout)
            (flyspell-incorrect :inherit             elegant-popout)

            ;; Ido
            (ido-first-match :inherit               elegant-salient)
            (ido-only-match  :inherit                 elegant-faded)
            (ido-subdir      :inherit                elegant-strong)

            ;; Diff
            (diff-header         :inherit             elegant-faded)
            (diff-file-header    :inherit            elegant-strong)
            (diff-context        :inherit                   default)
            (diff-removed        :inherit             elegant-faded)
            (diff-changed        :inherit            elegant-popout)
            (diff-added          :inherit           elegant-salient)
            (diff-refine-added   :inherit            elegant-strong)
            (diff-refine-changed :inherit            elegant-popout)
            (diff-refine-removed :inherit elegant-faded :strike-through t)

            ;; Term
            (term-bold          :inherit    elegant-strong)
            (term-color-black   :inherit    default)
            (term-color-white   :foreground "white"   :background "white")
            (term-color-blue    :foreground "#42A5F5" :background "#BBDEFB")
            (term-color-cyan    :foreground "#26C6DA" :background "#B2EBF2")
            (term-color-green   :foreground "#66BB6A" :background "#C8E6C9")
            (term-color-magenta :foreground "#AB47BC" :background "#E1BEE7")
            (term-color-red     :foreground "#EF5350" :background "#FFCDD2")
            (term-color-yellow  :foreground "#FFEE58" :background "#FFF9C4")

            ;; dired
            (dired-directory    :weight bold)

            ;; org-agendas
            (org-agenda-calendar-event   :inherit             default)
            (org-agenda-calendar-sexp    :inherit       elegant-faded)
            (org-agenda-clocking         :inherit       elegant-faded)
            (org-agenda-column-dateline  :inherit       elegant-faded)
            (org-agenda-current-time     :inherit       elegant-faded)
            (org-agenda-date             :inherit     elegant-salient)
            (org-agenda-date-today       :inherit      elegant-strong)
            (org-agenda-date-weekend     :inherit       elegant-faded)
            (org-agenda-diary            :inherit       elegant-faded)
            (org-agenda-dimmed-todo-face :inherit       elegant-faded)
            (org-agenda-done             :inherit       elegant-faded)
            (org-agenda-filter-category  :inherit       elegant-faded)
            (org-agenda-filter-effort    :inherit       elegant-faded)
            (org-agenda-filter-regexp    :inherit       elegant-faded)
            (org-agenda-filter-tags      :inherit       elegant-faded)
            (org-agenda-restriction-lock :inherit       elegant-faded)
            (org-agenda-structure        :inherit       elegant-faded)

            ;; org mode
            (org-archived                   :inherit      elegant-faded)
            (org-block                      :inherit      elegant-faded)
            (org-block-begin-line           :inherit      elegant-faded)
            (org-block-end-line             :inherit      elegant-faded)
            (org-checkbox                   :inherit      elegant-faded)
            (org-checkbox-statistics-done   :inherit      elegant-faded)
            (org-checkbox-statistics-todo   :inherit      elegant-faded)
            (org-clock-overlay              :inherit      elegant-faded)
            ;; (org-code                       :inherit      elegant-faded)
            (org-code                       :inherit      elegant-faded
                                            :foreground       "#2188b6")
            (org-column                     :inherit      elegant-faded)
            (org-column-title               :inherit      elegant-faded)
            (org-date                       :inherit      elegant-faded)
            (org-date-selected              :inherit      elegant-faded)
            (org-default                    :inherit      elegant-faded)
            (org-document-info              :inherit      elegant-faded)
            (org-document-info-keyword      :inherit      elegant-faded)
            (org-document-title             :inherit      elegant-faded
                                            :weight bold)
            (org-done                       :inherit            default)
            (org-drawer                     :inherit      elegant-faded)
            (org-ellipsis                   :inherit      elegant-faded)
            (org-footnote                   :inherit      elegant-faded)
            (org-formula                    :inherit      elegant-faded)
            (org-headline-done              :inherit      elegant-faded)
            (org-latex-and-related          :inherit      elegant-faded)
            (org-level-1                    :inherit     elegant-strong)
            (org-level-2                    :inherit     elegant-strong)
            (org-level-3                    :inherit     elegant-strong)
            (org-level-4                    :inherit     elegant-strong)
            (org-level-5                    :inherit     elegant-strong)
            (org-level-6                    :inherit     elegant-strong)
            (org-level-7                    :inherit     elegant-strong)
            (org-level-8                    :inherit     elegant-strong)
            (org-link                       :inherit    elegant-salient)
            (org-list-dt                    :inherit      elegant-faded)
            (org-macro                      :inherit      elegant-faded)
            (org-meta-line                  :inherit      elegant-faded)
            (org-mode-line-clock            :inherit      elegant-faded)
            (org-mode-line-clock-overrun    :inherit      elegant-faded)
            (org-priority                   :inherit      elegant-faded)
            (org-property-value             :inherit      elegant-faded)
            (org-quote                      :inherit      elegant-faded)
            (org-scheduled                  :inherit      elegant-faded)
            (org-scheduled-previously       :inherit      elegant-faded)
            (org-scheduled-today            :inherit      elegant-faded)
            (org-sexp-date                  :inherit      elegant-faded)
            (org-special-keyword            :inherit      elegant-faded)
            (org-table                      :inherit            default)
            (org-tag                        :inherit      elegant-faded)
            (org-tag-group                  :inherit      elegant-faded)
            (org-target                     :inherit      elegant-faded)
            (org-time-grid                  :inherit      elegant-faded)
            (org-todo                       :inherit     elegant-popout)
            (org-upcoming-deadline          :inherit      elegant-faded)
            (org-verbatim                   :inherit      elegant-faded)
            (org-verse                      :inherit      elegant-faded)
            (org-warning                    :inherit     elegant-popout)
            ;; (setq org-hide-emphasis-markers t)

            ;; Mu4e
            (mu4e-attach-number-face          :inherit   elegant-strong)
            (mu4e-cited-1-face                :inherit    elegant-faded)
            (mu4e-cited-2-face                :inherit    elegant-faded)
            (mu4e-cited-3-face                :inherit    elegant-faded)
            (mu4e-cited-4-face                :inherit    elegant-faded)
            (mu4e-cited-5-face                :inherit    elegant-faded)
            (mu4e-cited-6-face                :inherit    elegant-faded)
            (mu4e-cited-7-face                :inherit    elegant-faded)
            (mu4e-compose-header-face         :inherit    elegant-faded)
            (mu4e-compose-separator-face      :inherit    elegant-faded)
            (mu4e-contact-face                :inherit  elegant-salient)
            (mu4e-context-face                :inherit    elegant-faded)
            (mu4e-draft-face                  :inherit    elegant-faded)
            (mu4e-flagged-face                :inherit    elegant-faded)
            (mu4e-footer-face                 :inherit    elegant-faded)
            (mu4e-forwarded-face              :inherit    elegant-faded)
            (mu4e-header-face                 :inherit          default)
            (mu4e-header-highlight-face       :inherit   elegant-subtle)
            (mu4e-header-key-face             :inherit   elegant-strong)
            (mu4e-header-marks-face           :inherit    elegant-faded)
            (mu4e-header-title-face           :inherit   elegant-strong)
            (mu4e-header-value-face           :inherit          default)
            (mu4e-highlight-face              :inherit   elegant-popout)
            (mu4e-link-face                   :inherit  elegant-salient)
            (mu4e-modeline-face               :inherit    elegant-faded)
            (mu4e-moved-face                  :inherit    elegant-faded)
            (mu4e-ok-face                     :inherit    elegant-faded)
            (mu4e-region-code                 :inherit    elegant-faded)
            (mu4e-replied-face                :inherit  elegant-salient)
            (mu4e-special-header-value-face   :inherit          default)
            (mu4e-system-face                 :inherit    elegant-faded)
            (mu4e-title-face                  :inherit   elegant-strong)
            (mu4e-trashed-face                :inherit    elegant-faded)
            (mu4e-unread-face                 :inherit   elegant-strong)
            (mu4e-url-number-face             :inherit    elegant-faded)
            (mu4e-view-body-face              :inherit          default)
            (mu4e-warning-face                :inherit    elegant-faded)

            ;; ---------------- END package faces ----------------
            ))))

;; NOT used
(when elegant-modeline-disabled
  (let ((class '((class color) (min-colors 88) (background light))))
    (apply 'custom-theme-set-faces 'elegant-light
           (mapcar
            (lambda (x) `(,(car x) ((,class ,(cdr x)))))
            `((mode-line :inherit elegant-subtle :height 0.95)
              (mode-line-inactive :inherit mode-line :foreground nil)
              (mode-line-highlight :inherit mode-line :box t)
              (doom-modeline-bar :background "#333333")
              )))))

(provide 'elegant-light-theme)
;;; elegant-light-theme.el ends here
