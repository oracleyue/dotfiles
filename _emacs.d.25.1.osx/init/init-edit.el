;; ================================================================
;; Editing Enhancement
;; ================================================================
;; Last modified on 15 Sep 2017


;; ----------------------------------------------------------------------
;; Notes of usage
;; ----------------------------------------------------------------------
;; move cursor to the top/middle/bottom of the current window: "M-r"
;; upcase/downcase/captalize word: "M-u", "M-l", "M-c"
;; tranpose/swap words: "M-t"
;; use "whitespace-mode" to show white spaces as dots
;; insert-file: "C-x i"
;; write-file, save-as: "C-x w"
;; open-file in read-only: "C-x C-r"
;; undo: "C-/", "C-_";   redo: "C-?", "M-_";  (default by /undo-tree/)
;; comment line or region :: "C-\"; uncomment: "M-\"
;; captalize/upper/lower words: "M-c/u/l"
;; encodings:   "utf-8-unix"
;;      - open a file with specific coding system: =revert-buffer-with-coding-system=
;;      - set a encoding system for saving file: =set-buffer-file-coding-system=
;;      - find out the current coding system used for opening and saving files:
;;          =describe-current-coding-system=
;;      - find out what encoding system was used to decode current file:
;;          =describe-variable= "buffer-file-coding-system"
;;      - list all available encodings with "M-x list-coding-systems"
;;      - declare a file with a particular character encoding:
;;          add "-*- coding: utf-8 -*-" in the first line of your files
;; =open-previous-line= :: "M-o"
;; =open-next-line= :: "C-o"
;; remove all except 1 space between characters :: "C-c SPC" (built-in: M-SPC)
;; zap-to-char :: "M-z [e]"  (kills from the current point to a character)
;; kill line backwards :: "M-0 C-k" (built-in) OR "C-<backspace>"
;; kill one sentence backwards :: "C-x Backspace"
;; word count in region :: "M-="
;; reread file on disk :: "s-u" (s: super/command); "C-x C-v"
;; toggle overwrite mode :: "M-x overwrite-mode"
;; mark rings to jump:
;;      - set mark :: "C-SPC C-SPC"
;;      - jump to previous mark :: "C-u C-SPC"  after it, just "C-SPC" to continue jumping
;;      - jump to mark saved in global-mark ring :: "C-x C-SPC"
;;      - exchange the cursor and the previous mark :: "C-x C-x"
;;      - use helm-all-mark-rings to show mark ring :: "C-c h SPC"
;; register to jump:
;;      - record the position of point in register [r] :: "C-x r SPC [r]"
;;      - jump to the position saved in register [r] :: "C-x r j [r]"
;;      - use helm-register to show the list :: "C-c h x"
;; register to copy/paste:
;;      - "C-x r s [R]" to copy the region in register
;;      - "C-x r r [R]" to copy the *rectangle* to register
;;      - "C-x r g [R]" to (paste) insert text saved in register
;; rectangle edit:
;;      - kill rectangle :: "C-x r k"
;;      - copy rectangle :: "C-x r M-w"
;;      - yank the last killed rectangle :: "C-x r y"
;;      - delete the region-rectangle :: "C-x r d"
;;      - open rectangle, shifting text right :: "C-x r o"
;;      - blank out rectangle :: "C-x r c"
;;      - prefix each line with a string :: "C-x r t"
;; kill rings to yank:
;;      - "C-w / M-w" to kill or copy the mark region or the current line; "C-y" to yank
;;      - show kill ring and select to yank :: "M-y"
;; mark the whole buffer: "C-x h"
;; join the current line to the above one: "M-^"
;; fill/unfill paragraph/region:
;;      - "M-q"/"M-Q"
;;      - set fill-column: "C-x f"
;; split long lines up to a prefixed length: use auto-fill mode
;;      - set fill-column :: "C-x f"
;;      - split paragraph or all in region :: "M-q"
;; align comments :: "M-x align", "M-x align-regexp" ("C-x M-a") (prefix "C-u" for options)
;; use iedit :: "C-;"
;; use multi-cursor:
;;      - select one word "C->", then hit "C-g" to place multiple cursors
;;      - to place cursors in front of each lines:
;;        select multiple lines, then hit "C-S-l C-S-l" to place cursors
;;        OR
;;        select nothing and hit "C->", then edit
;;      - (disabled) use "C-S-SPC" to mark rectangular region
;; set rectangular region :: "C-x SPC" (built-in)
;; back-to-indenentation :: "M-m"  ==  "C-a TAB"
;; indent region in python-mode
;;      - python-indent-shift-right "C-c >"
;;      - python-indent-shift-left  "C-c <"
;; add incremental numbers to lines :: "C-x r N" (built-in; now bound to new func)
;; use set-goal-column to do vertical editing :: "C-x C-n"; to remove "C-u C-x C-n"
;; insert date :: "M-x insert-date"
;; toggle window split (horizontal <-> vertical) :: "C-x |"
;; change encodings:   utf-8-unix, utf-8-dos
;;      - change when saving using "C-x C-m f"
;;      - force it immediately by using "C-x C-m c <encoding> RET C-x C-w RET"
;; change End-of-Line:
;;      - "M-x set-buffer-file-coding-system utf-8-unix" or use "C-x RET f"
;; open the *Messages* buffer :: "C-h e"
;; flyspell-auto-correct-word :: "C-."
;; ----------------------------------------------------------------------



;;
;; ------------- Basic Editing Extensions ---------------
;;

;; revert-buffer: update buffer if the file in disk has changed
;; default keybinding: "s-u" (s: super/win/command)
(global-set-key (kbd "s-u") 'revert-buffer)

;; if region marked, kill/copy region (default C-w/M-w); otherwise, kill/copy the current line
(defun y:kill-ring-save ()
        (interactive)
        (if (equal mark-active nil)
            ;;(kill-ring-save (point) (line-end-position)) ; current point TO end of line
            (kill-ring-save (line-beginning-position) (line-end-position))
          (kill-ring-save (point) (mark))))
(defun y:kill-region ()
        (interactive)
        (if (equal mark-active nil)
            ;;(kill-region (point) (line-end-position)) ; current point TO end of line
            (kill-region (line-beginning-position) (line-end-position))
          (kill-region (point) (mark))))
(global-set-key (kbd "M-w") 'y:kill-ring-save)
(global-set-key (kbd "C-w") 'y:kill-region)

;; mark the current line
(defun y:mark-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-S-SPC") 'y:mark-current-line)
;; "C-S-SPC" is bound to for =set-rectangular-region-anchor= in /multi-cursor/

;; remove all except 1 space between characters ("M-SPC" disabled due to Alfred)
(global-set-key (kbd "C-c SPC") 'just-one-space)

;; unfill paragraph: the opposite of fill-paragraph
(defun y:unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'y:unfill-paragraph-or-region)

;; open a new line and jump there
(require 'open-next-line)
(global-set-key (kbd "C-o") 'open-next-line)

;; enable editing or replacing when region is active, e.g. yank
(delete-selection-mode 1)

;; kill line backwards
(defun y:backward-kill-line ()
  (interactive)
  (if visual-line-mode
      (kill-visual-line 0)
    (kill-line 0)
    (indent-according-to-mode)))
(global-set-key (kbd "C-<backspace>") 'y:backward-kill-line)

;; align-regexp keybinding
(global-set-key (kbd "C-x M-a") 'align-regexp)


;;
;; ------- More Editing-related Extensions ---------
;;

;; key bindings for comment/uncomment
(defun y:comment-line-or-region (&optional beg end)
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
(defun y:uncomment-line-or-region (&optional beg end)
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
;; (defun y:comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (let (beg end)
;;         (if (region-active-p)
;;             (setq beg (region-beginning) end (region-end))
;;             (setq beg (line-beginning-position) end (line-end-position)))
;;         (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-\\") 'y:comment-line-or-region)
(global-set-key (kbd "M-\\") 'y:uncomment-line-or-region)
(global-set-key (kbd "C-|") 'y:uncomment-line-or-region)   ;; invalid in terminals

;; fix undo/redo using /undo-tree.el/, if not using /Evil/
(global-undo-tree-mode)

;; toggle window split between horizontal-split and vertical-split
(defun y:toggle-window-split ()
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
(global-set-key (kbd "C-x |") 'y:toggle-window-split)

;; insert date
(defvar current-date-format "%d %b %Y")  ;; %a" for weekdays
(defvar current-time-format "%H:%M:%S")
(defun insert-date ()
  "insert the current date and time into current buffer.
Uses `current-date-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))
(defun insert-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

;; adding incremental numbers to lines
(require 'gse-number-rect)
(global-set-key (kbd "C-x r N") 'gse-number-rectangle)

;; removing trailing whitespace
;(add-hook 'before-save-hook 'delete-trailing-whitespace)   ;; any files
;; enable in all programming modes
(defun y:auto-remove-trailing-whitespace ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'y:auto-remove-trailing-whitespace)


;;
;; ----------- Powerful Minor Modes ------------
;;

;; /multiple-cursors/: edit with multiple cursors
(require 'multiple-cursors)
;; mark many occurrences in region
(global-set-key (kbd "C-S-l C-S-l") 'mc/edit-lines)  ;; default (C-S-c C-S-c)
;; mark one more occurrence by regexp match
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; mouse events
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
;; others
;(global-set-key (kbd "M-S-SPC") 'set-rectangular-region-anchor)


;; /expand-region/: increase the selected region by semantic units
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;
;; ----------- Settings of /text-mode/ ------------
;;
(setq-default major-mode 'text-mode)
;; use "C-." to auto-correct words

;; line wrapping settings
(add-hook 'text-mode-hook 'visual-line-mode)

;; spell check for text modes
(add-hook 'text-mode-hook 'flyspell-mode)
;; (setq ispell-dictionary "british")
(setq ispell-dictionary "american")

;; use /auto-fill mode/
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)


;;
;; ------------------- Keybindings for Terminals -------------------
;;
(unless (display-graphic-p)
  (require 'open-next-line)   ;; if not using /emacs-python/
  (global-set-key (kbd "C-o") 'open-next-line))



(provide 'init-edit)
;; ================================================
;; init-edit.el ends here
