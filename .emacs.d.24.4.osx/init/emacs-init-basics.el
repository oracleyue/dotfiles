;;; Fundemental configrations

;; ----------------------------------------------------------------------
;; BASIC USAGES
;; ---------------
;; undo: "C-/", "C-_";   redo: "C-?", "M-_";   (default by /undo-tree/)
;; captalize/upper/lower words: "M-c/u/l"
;; changing encodings: "C-x C-m f"
;; =open-previous-line= :: "M-o"
;; =open-next-line= :: "C-o"
;; remove all except 1 space between characters :: "C-c SPC" (built-in: M-SPC)
;; zap-to-char :: "M-z [e]"  (kills from the current point to a character)
;; kill line backwards :: "M-0 C-k" (built-in) OR "C-<backspace>"
;; kill one sentence backwards :: "C-x Backspace"
;; comment line or region :: "C-\"
;; uncomment line or region :: "C-S-\"
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
;; join lines into one:
;;      - join two lines: "M-^"
;;      - join multiple lines in region: "C-^"
;; fill/unfill paragraph:
;;      - "M-q"/"M-Q"
;;      - set fill-column: "C-x f"
;; split long lines up to a prefixed length: use auto-fill mode
;;      - set fill-column :: "C-x f"
;;      - split paragraph or all in region :: "M-q"
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
;; open default Dired folders on startup :: "M-x y:dired-open-folders-startup"
;; toggle window split (horizontal <-> vertical) :: "C-x |"
;;
;; <Saturday, January 28, 2017>
;; ----------------------------------------------------------------------



;;
;; Editing Enhancement
;;

;; Note: some keybindings are added at the end of .emacs, due to the complication to locate which third packages change the original keybindings

;; revert-buffer: update buffer if the file in disk has changed
;(defun y:revert-buffer-no-confirm ()
;    "Revert buffer without confirmation."
;    (interactive)
;    (revert-buffer t t)
;    (minibuffer-message "File changed on disk. Reread from disk."))
;; default keybinding: "s-u"
;; (global-set-key (kbd "C-x C-v") 'revert-buffer)
;; (global-set-key (kbd "C-x C-v") 'y:revert-buffer-no-confirm)

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

;; join mutiple lines in region
(defun y:join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))
(global-set-key (kbd "C-^") 'y:join-region)

;; unfill paragraph: the opposite of fill-paragraph
(defun y:unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'y:unfill-paragraph)

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



;;
;; Interface Configuration
;;
(load (concat y-init-path-prefix "emacs-init-theme"))


;;
;; Other Settings
;;

;; use ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; increase memeory for emacs to avoid garbage collections slow it down
(setq gc-cons-threshold (* 20 1024 1024))   ; 20MB, default <0.8MB

;; configure mark-ring
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 16)
(setq global-mark-ring-max 32)

;; ;; use variable-width font types in text-mode
;; (defun y-variable-width-text-mode ()
;;   (interactive)
;;   (variable-pitch-mode t)
;;   (text-scale-increase 0.5)
;;   )
;; (add-hook 'text-mode-hook 'y-variable-width-text-mode)

;; set cursor type: "bar", "box" (default)
;(setq-default cursor-type 'bar)

;; font size adjustment
;; C-x C-0 : return to default size
;; use C-x C-0 first, then use +/- to tune the size.
;(global-set-key (kbd "C-x C-=") (lambda () (interactive) (text-scale-increase 0.5)))
;(global-set-key (kbd "C-x C--") (lambda () (interactive) (text-scale-decrease 0.5)))


;; use Command as Control in Mac OS X for emacs, if not like to swap Command and Control
(cond
 ((string-equal system-type "darwin")
  ;; (setq mac-command-modifier 'control)  ; use Command key also as Control
  ;; (setq mac-option-modifier 'meta)  ; NOT need
  ))

;; fix $PATH for emacs in Mac OS X
(defun y-mac:set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    ;; (setenv "PATH" path-from-shell)
    (setenv "PATH" (concat "~/.emacs.d/bin:" path-from-shell))
    (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages/")
    ;; (setq exec-path (split-string path-from-shell path-separator))
    (setq exec-path (split-string (getenv "PATH") path-separator))))
(defun y-linux:set-exec-path-from-shell-PATH()
  (setenv "PATH" (concat "~/.emacs.d/bin:" (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") path-separator)))
(when (string-equal system-type "darwin") (y-mac:set-exec-path-from-shell-PATH))
(when (string-equal system-type "gnu/linux") (y-linux:set-exec-path-from-shell-PATH))

;; settings for graphic or terminal modes
(if (display-graphic-p)
    (menu-bar-mode 1))
(if (not (display-graphic-p))
    (menu-bar-mode 1))   ; -1 to disable

;; setting font set for Chinese
(if(display-graphic-p)
 (dolist (charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font)
                      charset
                     (font-spec :family "WenQuanYi Micro Hei" :size 12)))
)
;; various one line commands/config, like "TAB"
(setq-default tab-width 4)  ; control the width of a literal tab (C-q TAB; key=9)
(setq-default indent-tabs-mode nil)  ; use spaces instead of evil tabs, width controled by "tab-stop-list"

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

;;; disable all version control. makes startup and opening files much faster
;(setq vc-handled-backends nil)

;;; setting the url brower for emacs
;; (setq browse-url-browser-function 'browse-url-firefox
;;       browse-url-new-window-flag  t
;;       browse-url-firefox-new-window-is-tab t)

;; set default major mode to text-mode
(setq-default major-mode 'text-mode)

;; spell checking for some modes
(add-hook 'text-mode-hook 'flyspell-mode)
;; (setq ispell-dictionary "british")
(setq ispell-dictionary "american")

;; oracleyue's inital Dired folders on startup
(setq y:HomePath "/Users/oracleyue/")
(defun y:dired-open-folders-startup ()
  (interactive)
  "Setup the startup folders. Used in .emacs"
  ;(dired (concat y:HomePath "Public/Dropbox/Workspace/matlab"))
  (dired (concat y:HomePath "Public/Dropbox/oracleyue/OrgNote"))
  ;(dired (concat y:HomePath "Public/Dropbox/Academia"))
  ;(dired (concat y:HomePath "Public/Dropbox/Academia/Manuscripts"))
  (dired (concat y:HomePath "Public/Dropbox/Academia/Seminars"))
  ;(dired (concat y:HomePath "Public/Dropbox/Shared"))
  (find-file (concat y:HomePath "Public/Dropbox/Academia/ToDoList.org"))
  (switch-to-buffer "*scratch*"))
(defun email ()
  (interactive)
  (find-file (concat y:HomePath "Documents/email.tmp.md"))
  (set-fill-column 75))

;; oracleyue's inital path setting
(defun y:set-startup-directory ()
  (cd (concat y:HomePath "/Public/Dropbox/Academia/Manuscripts")))
    ;; For Ubuntu@LCSB
    ;(setq default-directory (concat y:HomePath "/Workspace/matlab/"))

;; oracleyue's env. variables and alias
;(setenv "MATLAB_JAVA" "/usr/lib/jvm/java-7-openjdk/jre")
    ; For Ubuntu@LCSB
    ; (setenv "MATLAB_JAVA" "/usr/lib/jvm/java-7-openjdk-amd64/jre")

;;; environment variables for shell
;; ;; Shell mode
;; (setq ansi-color-names-vector ; better contrast colors
;;       ["black" "red4" "green4" "yellow4"
;;        "blue3" "magenta4" "cyan4" "white"])
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; line wrapping settings
;(define-key global-map [f4] 'toggle-truncate-lines)
(add-hook 'text-mode-hook 'visual-line-mode)

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
(defun y:comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-\\") 'y:comment-line-or-region)
(global-set-key (kbd "C-|") 'y:uncomment-line-or-region)   ;; invalid in terminals
(global-set-key (kbd "M-\\") 'y:comment-or-uncomment-region-or-line)

;; fix undo/redo using /undo-tree.el/, if not using /Evil/
(global-undo-tree-mode)

;; configure /hl-line-mode/ for /monokai/, enabled in python-mode
    ;; to highlight the single row where the cursor is.
;; configure /highlight-indentation/ for /monokai/, enabled in python-mode
    ;; to highlight indentations
    ;; NOT work! Having to be and having been set in .emacs

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
;(global-set-key (kbd "C-c M-d") 'insert-date)
;; simple formated dates
(defun insert-date-simple (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))
;; another version: formated string
(defun insdate-insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))
(defun insert-date (&optional days)
  "Insert date that is DAYS from current."
  (interactive "p*")
  (unless days (setq days 0))
  (insert
   (calendar-date-string
    (calendar-gregorian-from-absolute
     (+ (calendar-absolute-from-gregorian (calendar-current-date))
        days)))))

;; adding incremental numbers to lines
(require 'gse-number-rect)
(global-set-key (kbd "C-x r N") 'gse-number-rectangle)

;; removing trailing whitespace
;(add-hook 'before-save-hook 'delete-trailing-whitespace)   ;; any files
;; enable in all programming modes
(defun y:auto-remove-trailing-whitespace ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'y:auto-remove-trailing-whitespace)
