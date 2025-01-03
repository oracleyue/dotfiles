;; ================================================================
;; Editing Enhancement
;; ================================================================
;; Last modified on 15 Sep 2017

;; ------------- Basic Editing Extensions ---------------

;; kill sentence (default use doulbe space after the period)
(setq sentence-end-double-space nil)
;; kill-line: =C-k=; kill-sentence: =M-k=
;; (global-set-key (kbd "C-S-k") 'kill-paragraph)

;; revert-buffer: update buffer if the file in disk has changed
;; default keybinding: "s-u" (s: super/win/command)
(global-set-key (kbd "s-u") 'revert-buffer)

;; if region marked, kill/copy region (default C-w/M-w); otherwise, kill/copy the current line
(defun zyue/kill-ring-save ()
        (interactive)
        (if (equal mark-active nil)
            ;;(kill-ring-save (point) (line-end-position)) ; current point TO end of line
            (kill-ring-save (line-beginning-position) (line-end-position))
          (kill-ring-save (point) (mark))))
(defun zyue/kill-region ()
        (interactive)
        (if (equal mark-active nil)
            ;;(kill-region (point) (line-end-position)) ; current point TO end of line
            (kill-region (line-beginning-position) (line-end-position))
          (kill-region (point) (mark))))
(global-set-key (kbd "M-w") 'zyue/kill-ring-save)
(global-set-key (kbd "C-w") 'zyue/kill-region)

;; unfill paragraph or region: the opposite of fill-paragraph or fill-region "M-q"
(defun zyue/unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-Q") 'zyue/unfill-paragraph-or-region)

;; open a new line and jump there
(use-package open-next-line
  :ensure nil
  :load-path "site-lisp"
  :bind (("C-o" . open-next-line)
         ("M-o" . open-previous-line)))

;; enable editing or replacing when region is active, e.g. yank
(delete-selection-mode 1)

;; kill line backwards
(defun zyue/backward-kill-line ()
  (interactive)
  (if visual-line-mode
      (kill-visual-line 0)
    (kill-line 0)
    (indent-according-to-mode)))
(global-set-key (kbd "C-<backspace>") 'zyue/backward-kill-line)

;; align-regexp keybinding
;; one may prefix "C-u" for more arguments.
(global-set-key (kbd "C-x M-a") 'align-regexp)

;; ------- More Editing-related Extensions ---------

;; Comment/Uncomment Functions:
;; - "M-;" comment-dwim
;; - "C-x C-;": comment-line
;; enhancements for comment/uncomment (check also newcommenter.el)
(defun zyue/comment-line-or-region (&optional beg end)
  (interactive)
  (let ((beg (cond (beg beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (comment-region beg end)))
(defun zyue/uncomment-line-or-region (&optional beg end)
  (interactive)
  (let ((beg (cond (beg beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (uncomment-region beg end)))
(global-set-key (kbd "C-\\") 'zyue/comment-line-or-region)
;; (global-set-key (kbd "M-\\") 'zyue/uncomment-line-or-region)
(global-set-key (kbd "C-|") 'zyue/uncomment-line-or-region)   ;; invalid in terminals

;; insert date
(defun zyue/insert-today ()
  "insert the current date and time into current buffer.
Uses `current-date-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string "%d %b %Y" (current-time))))
(defun zyue/insert-time ()
  "insert current time in the format 12:37:05 2020-01-31."
  (interactive)
  (insert (format-time-string "%H:%M:%S %Y-%m-%d" (current-time))))

;; adding incremental numbers to lines
(require 'gse-number-rect)
(global-set-key (kbd "C-x r N") 'gse-number-rectangle)

;; remove all except 1 following space (default "M-SPC" disabled due to Alfred)
;; alternative way to "M-SPC": "Esc SPC", or use
;; (global-set-key (kbd "M-s SPC") 'just-one-space)

;; removing trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; automatic whitespace cleanup
(defun auto-cleanup-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'auto-cleanup-whitespace)
(add-hook 'LaTeX-mode-hook #'auto-cleanup-whitespace)
;; manually remove whitespaces (buffer or region)
(global-set-key (kbd "M-s k") 'delete-trailing-whitespace)

;; remote blank lines
;; default "C-x C-o" to call "delete-blank-lines"

;; ----------- Powerful Minor Modes ------------

;; /undo-tree/: better undo/redo using tree visualizer
(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        ;; show diff window (use "d" to toggle)
        undo-tree-visualizer-diff t)
  :bind (("C-x u" . undo-tree-visualize)))
;; usage: "q" to quit, "C-q" to abort, "d" to toggle diff

;; /multiple-cursors/: edit with multiple cursors
(use-package multiple-cursors
  :demand
  :bind (;; mark many occurrences in region
         ("C-S-l C-S-l"   . mc/edit-lines) ;; default (C-S-c C-S-c)
         ;; mark one more occurrence by regexp match
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ;; mouse events
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ;; others
         ("C-S-SPC"       . set-rectangular-region-anchor)))

;; /expand-region/: increase the selected region by semantic units
(use-package expand-region
  :demand
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; /rainbow-delimiters/: highlight parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; /Avy/: jump to char/words in tree-style
(use-package avy
  :demand
  :bind (("M-g c"   . avy-goto-char)       ; default: C-:
         ("M-g w"   . avy-goto-word-1)     ; avy-goto-word-0: too many candiates
         ;("M-g M-r" . avy-resume)
         ("M-g g"   . avy-goto-line)       ; overwritten by "consult-goto-line"
         ("M-g M-g" . avy-goto-line))
  :config
  (avy-setup-default)
  ;; restore key overwritten by "Consult" (init-vertico.el)
  :hook (after-init . (lambda () (global-set-key (kbd "M-g M-g") 'avy-goto-line))))


(provide 'init-edit)
;; ================================================
;; init-edit.el ends here
