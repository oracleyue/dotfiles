;; -------------------------------------------------------------------
;; Additional minimal Emacs native configurations to assist elegant-theme
;; Copyright 2020 Nicolas P. Rougier
;; -------------------------------------------------------------------

(require 'elegant-light-theme)

;;; Font and frame size
(setq default-frame-alist
      (append (list '(internal-border-width . 24)
                    '(width  . 81) '(height . 46)
                    '(font . "Roboto Mono 14")
                    '(vertical-scroll-bars . nil)
                    '(bottom-divider-width . 1))
              default-frame-alist))

;;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq widget-image-enable nil)

;;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; No Tooltips
(tooltip-mode 0)

;;; Paren mode is part of the theme
(show-paren-mode t)

;;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

;;; Mode line rendering
(defun elegant-set-modeline ()
  "Mode line default used in elegant-emacs."

  ;; this line below makes things a bit faster
  (set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

  (define-key mode-line-major-mode-keymap [header-line]
    (lookup-key mode-line-major-mode-keymap [mode-line]))

  (defun mode-line-render (left right)
    "Function to render the modeline LEFT to RIGHT."
    (let* ((available-width (- (window-width) (length left) )))
      (format (format "%%s %%%ds" available-width) left right)))

  (setq-default mode-line-format
                '((:eval
                   (mode-line-render
                    (format-mode-line (list
                                       (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                   'help-echo "Mode(s) menu"
                                                   'mouse-face 'mode-line-highlight
                                                   'local-map   mode-line-major-mode-keymap)
                                       " %b "
                                       (if (and buffer-file-name (buffer-modified-p))
                                           (propertize "(modified)" 'face `(:inherit elegant-faded)))))
                    (format-mode-line
                     (propertize "%4l:%2c" 'face `(:inherit modeline)))))))

  ;; set modeline at the top
  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format '("")))

;;; Modeline faces
(defun elegant-set-modeline-faces ()
  "Mode line at top."
  (set-face 'header-line 'elegant-strong)
  (set-face-attribute 'header-line nil
                      :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive 'mode-line)
  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-foreground 'mode-line)
                      :background (face-background 'mode-line))
  )

;; load modeline setup (use headline instead)
(unless elegant-modeline-disabled
  (elegant-set-modeline-faces)
  (elegant-set-modeline))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


(provide 'elegant-theme)
;; ----------------------------------------------------------------
;; elegant-theme.el ends here
