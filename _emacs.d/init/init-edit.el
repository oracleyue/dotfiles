;; ================================================================
;; Editing Enhancement
;; ================================================================
;; Last modified on 15 Sep 2017

;; ------------- Basic Editing Extensions ---------------

;; kill sentence (default use doulbe space after the period)
(setq sentence-end-double-space nil)
;; kill-line: =C-k=; kill-sentence: =M-k=
(global-set-key (kbd "C-S-k") 'kill-paragraph)

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

;; unfill paragraph or region: the opposite of fill-paragraph or fill-region
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

;; key bindings for comment/uncomment
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
;; (defun zyue/comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (let (beg end)
;;         (if (region-active-p)
;;             (setq beg (region-beginning) end (region-end))
;;             (setq beg (line-beginning-position) end (line-end-position)))
;;         (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-\\") 'zyue/comment-line-or-region)
(global-set-key (kbd "M-\\") 'zyue/uncomment-line-or-region)
(global-set-key (kbd "C-|") 'zyue/uncomment-line-or-region)   ;; invalid in terminals

;; fix undo/redo using /undo-tree.el/, if not using /Evil/
(use-package undo-tree
  :demand
  :diminish
  :config (global-undo-tree-mode))

;; toggle window split between horizontal-split and vertical-split
(defun zyue/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x |") 'zyue/toggle-window-split)

;; insert date
(defun zyue/insert-date ()
  "insert the current date and time into current buffer.
Uses `current-date-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string "%d %b %Y" (current-time))))
(defun zyue/insert-today ()
  "insert today in the format 2020-01-31."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))
(defun zyue/insert-time ()
  "insert the current time (format e.g. 12:37:05) into the current buffer."
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))
(defun zyue/iso-8601-date-string ()
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

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


(provide 'init-edit)
;; ================================================
;; init-edit.el ends here
