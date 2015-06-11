;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ---------------------------
;;
;; Spolsky : A dark color theme
;;
;; ----------------------------

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme yspolsky  "A dark color theme for Emacs based on Sublime Text 2")

(custom-theme-set-variables
  'yspolsky
  ;; '(linum-format " %7i "))
  '(linum-format " %3i "))

(defun in-terminal ()
  "Return true if in a terminal."
  (not (display-graphic-p)))

(let (;(*background*         "#161A1F")
      (*background*         (if (in-terminal) "#303030" "#272822"))
      (*comments*           "#8C8C8C")
      (*constant*           "#FF80F4")
      (*current-line*       "#151515")
      (*cursor-underscore*  "#EEDC82")
      (*keywords*           "#F92672")

      ;; Sidebar line numbers
      ;(*line-number*        "#161A1F")
      (*line-number*        "#444444")  ; by oracleyue
      ;(*line-fg*            "#666")
      (*line-fg*            "#626262")  ; by oracleyue

      (*type-face*          "#66D9EF")
      (*method-declaration* "#A6E22E")
      (*mode-line-bg*       "#333")
      (*mode-inactive-bg*   "#222")
      (*mode-line-fg*       "#EEDC82")
      (*mode-inactive-fg*   "#555")
      (*normal*             "#DEDEDE")
      (*number*             "#FC580C")
      (*operators*          "#FF80F4")
      (*warning*            "#FF6C60")
      (*regexp*             "#A63A62")
      (*string*             "#EEDC82")
      (*variable*           "#FD971F")
      (*visual-selection*   "#555")
      (*region-bg*          "#005F87")  ; by oracleyue
      (*isearch-bg*  "#5FD7D7")  ; by oracleyue
      (*isearch-fg*  "#303030")  ; by oracleyue
      ;; (*lazy-highlight-bg*  "#5FD7D7")  ; by oracleyue
      ;; (*lazy-highlight-fg*  "#303030")  ; by oracleyue
)

  (custom-theme-set-faces
   'yspolsky

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   ;`(region ((t (:background, *visual-selection*))))
   `(region ((t (:background, *region-bg*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*, :bold, t))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*, :bold, t))))
   `(font-lock-negation-char-face ((t (:foreground, *warning*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *type-face*, :bold, t))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *warning*, :bold, t))))

   ;; GUI
   `(fringehl ((t (:background, *line-fg*))))
   ;; `(fringe ((t (:background, *background*))))
   `(fringe ((t (:background, *line-fg*))))
   `(linum ((t (:background, *line-number* :foreground, *line-fg*))))
   ;; `(minibuffer-prompt ((t (:foreground, *variable*, :bold, t))))
   `(minibuffer-prompt ((t (:foreground, *isearch-bg*, :bold, t))))
   `(dired-header ((t (:bold, t, :foreground, *isearch-bg*)))) ; add by oracleyue
   `(widget-button ((t (:bold, t, :foreground, *isearch-bg*)))); add by oracleyue
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *cursor-underscore*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *background*)))) ;; between splits

;;; -----------------------------------------------------------------
   ;; Appearance of org-mode
   `(org-block-begin-line
     ((t (:height 1.0 :weight bold :slant italic :underline "#A6A6AA" :foreground "#C2C2C2" :background "#444444"))))
   ;; `(org-block-background
   ;;      ((t (:background "#FFFFEA"))))
   `(org-block-end-line
        ((t (:height 1.0 :weight bold :slant italic :overline "#A7A6AA" :foreground "#C2C2C2" :background "#444444"))))
   ; ---
   ;; `(org-document-title ((t (:family "DejaVu Sans" :height 1.4 :weight bold :foreground "#626262"))))
   ;; `(org-level-1 ((t (:family "DejaVu Sans" :height 1.0 :weight bold :overline "#A7A7A7" :background "#626262"))))
   ;; `(org-level-2 ((t (:family "DejaVu Sans" :height 0.9 :weight bold :overline "#123555" :background "#626262"))))
   ;; `(org-level-3 ((t (:height 1.0 :weight bold :overline "#005522" :background "#626262"))))
   ;; `(org-level-4 ((t (:height 1.0 :weight bold))))
   ;; `(org-level-5 ((t (:height 1.0 :weight bold :slant normal))))
   ;; `(org-level-6 ((t (:height 1.0 :weight bold :slant italic))))
   ;; `(org-level-7 ((t (:height 1.0 :weight bold :slant italic))))
   ;; `(org-level-8 ((t (:height 1.0 :weight bold :slant italic))))
;;; -----------------------------------------------------------------
   
   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *keywords* :foreground, *normal* :weight bold))))

   ;; search
   ;`(isearch ((t (:background, *regexp* :foreground, *visual-selection*))))
   `(isearch ((t (:background, *isearch-bg* :foreground, *isearch-fg*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))
   ;; `(lazy-highlight ((t (:background, *lazy-highlight-bg* :foreground, *lazy-highlight-fg*))))   ; by oracleyue

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yspolsky)

;; Local Variables:
;; no-byte-compile: t
;; End:


