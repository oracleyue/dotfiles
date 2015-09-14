;;; Fundemental configrations

;; ----------------------------------------------------------------------
;; BASIC USAGES
;; ---------------
;; =open-previous-line= :: "M-o"
;; =open-next-line= :: "C-o"
;; kill backwards to the beginning of current line :: "M-0 C-k"
;; comment line or region :: "C=\"
;; uncomment line or region :: "C-S-\"
;; toggle overwrite mode :: "M-x overwrite-mode"
;; mark rings to jump:
;;      - set mark :: "C-SPC C-SPC"
;;      - jump to previous mark :: "C-u C-SPC"
;;      - show mark ring and select to jump :: "C-c h SPC"
;; kill rings to yank:
;;      - "C-w / M-w" to kill or copy; "C-y" to yank
;;      - show kill ring and select to yank :: "M-y"
;; use iedit :: "C-;"
;; use multi-cursor:
;;      - select one word "C->", then hit "C-g" to place multiple cursors
;;      - to place cursors in front of each lines:
;;        select multiple lines, then hit "C-S-l C-S-l" to place cursors
;;        OR
;;        select nothing and hit "C->", then edit
;;      - use "C-S-SPC" to mark rectangular region (replace default "C-x r ..." operation)
;; indent region in python-mode
;;      - python-indent-shift-right "C-c >"
;;      - python-indent-shift-left  "C-c <"
;; ----------------------------------------------------------------------

;;
;; Basic Emacs Operation Enhancement
;;
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
;;
(defun y:mark-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
;(global-set-key (kbd "C-S-SPC") 'y:mark-current-line)
;; "C-S-SPC" is reserved for =set-rectangular-region-anchor= in /multi-cursor/


;;
;; Face Enhancement
;; 

;; setting size of frames
;(when window-system (set-frame-size (selected-frame) 100 36))
(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(width . 100))

;; Using default theme
;(load-theme 'deeper-blue t)
;(load-theme 'adwaita t)       ;grey
;; Using oracleyue's theme
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'ymonokai t)
;(load-theme 'monokai t)
;; Fringe setting (right-only); bug: cause linum-mode to destory the auto-complete popup menu
;(fringe-mode '(0 . nil))

;; set mode-line
(make-face 'mode-line-linum-face-y)
(make-face 'mode-line-buffer-name-face-y)
(make-face 'mode-line-plain-face-y)
(set-face-attribute 'mode-line-linum-face-y nil
                    :foreground "#66D9EF")
(set-face-attribute 'mode-line-buffer-name-face-y nil
                    :foreground "#A6E22E"
                    :bold t)
(set-face-attribute 'mode-line-plain-face-y nil
                    :foreground "#F8F8F2")
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; default part
    "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification
    ;mode-line-buffer-identification
    
    ;; the buffer name; the file name as a tool tip (default: 'face 'font-lock-keyword-face)
    '(:eval (propertize "%b " 'face 'mode-line-buffer-name-face-y
        'help-echo (buffer-file-name)))
    
    ;; line and column (default: 'face 'font-lock-type-face)
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'mode-line-linum-face-y) ","
      (propertize "%02c") 
    ") "

    ;; relative position, size of file (default: 'face 'font-lock-constant-face)
    "["
    (propertize "%p") ;; % above top
    "/"
    (propertize "%I") ;; size
    "] "

    ;; the current major mode for the buffer (default: 'face 'font-lock-string-face)
    "["

    '(:eval (propertize "%m"
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                        'face
                        (if overwrite-mode 'font-lock-preprocessor-face
                                           'mode-line-plain-face-y)
                        ;'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save? 
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    "  --"
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    "--"
    
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    
    ;"%-" ;; fill with '-'
    ))



;;
;; other settings
;;

;; increase memeory for emacs to avoid garbage collections slow it down
(setq gc-cons-threshold (* 20 1024 1024))   ; 20MB, default <0.8MB

;;; use variable-width font types in text-mode
;; (defun y-variable-width-text-mode ()
;;   (interactive)
;;   (variable-pitch-mode t)
;;   (text-scale-increase 0.5)
;;   )
;(add-hook 'text-mode-hook 'y-variable-width-text-mode)

;; set cursor type; default "box"
;(setq-default cursor-type 'bar) 

;; font size adjustment
;; C-x C-0 : return to default size
;;; use C-x C-0 first, then use +/- to tune the size.
;(global-set-key (kbd "C-x C-=") (lambda () (interactive) (text-scale-increase 0.5)))
;(global-set-key (kbd "C-x C--") (lambda () (interactive) (text-scale-decrease 0.5)))


;; Use Command as Control in Mac OS X for emacs, if not like to swap Command and Control 
(cond 
 ((string-equal system-type "darwin")
  ;(setq mac-command-modifier 'control)  ; use Command key also as Control
  ;(setq mac-option-modifier 'meta)
  ))

;; Fix $PATH for emacs in Mac OS X
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

;; Settings for graphic or terminal modes
(if (display-graphic-p)
    (menu-bar-mode 1))
(if (not (display-graphic-p))
    (menu-bar-mode -1))

;; Setting font set for Chinese
(if(display-graphic-p)
 (dolist (charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font)
                      charset
                     (font-spec :family "WenQuanYi Micro Hei" :size 12)))
)
;; Various one line commands/config, like "TAB"
(setq-default tab-width 4)  ; control the width of a literal tab (C-q TAB; key=9)
(setq-default indent-tabs-mode nil)  ; use spaces instead of evil tabs, width controled by "tab-stop-list"

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

;;; Disable all version control. makes startup and opening files much faster
;(setq vc-handled-backends nil)

;;; Setting the url brower for emacs
;; (setq browse-url-browser-function 'browse-url-firefox
;;       browse-url-new-window-flag  t
;;       browse-url-firefox-new-window-is-tab t)

;; Set default major mode to text-mode
(setq-default major-mode 'text-mode)

;;; Spell Checking for some modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (setq ispell-dictionary "british")
(setq ispell-dictionary "american")
(add-hook 'LaTeX-mode-hook 'ispell)


;; oracleyue's inital path setting
;(cd "~/Public/Dropbox/oracleyue/OrgNote")
;(cd "~/Public/Dropbox/Academia/matlab")
(cd "~/Public/Dropbox/Academia/Manuscripts/DSF")
;(cd "~/Workspace/matlab")
    ;; For Ubuntu@LCSB 
    ;(setq default-directory "~/Workspace/matlab/Feng_prj_HPC")

;; oracleyue's env. variables and alias
;(setenv "MATLAB_JAVA" "/usr/lib/jvm/java-7-openjdk/jre")
    ; For Ubuntu@LCSB
    ; (setenv "MATLAB_JAVA" "/usr/lib/jvm/java-7-openjdk-amd64/jre")
(setenv "orgnote" "~/Public/Dropbox/oracleyue/OrgNote")
(setenv "gitdoc" "~/Public/Dropbox/Academia/Manuscripts")
(setenv "github" "~/Worksapce/github.com")
(setenv "gitrepo" "~/Worksapce/gitrepo")

;;; oracleyue's env. variables to control shell
;; ;; Shell mode
;; (setq ansi-color-names-vector ; better contrast colors
;;       ["black" "red4" "green4" "yellow4"
;;        "blue3" "magenta4" "cyan4" "white"])
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; oracleyue's line wrapping settings
;(define-key global-map [f4] 'toggle-truncate-lines)
(add-hook 'text-mode-hook 'visual-line-mode)

;; oracleyue's key bindings
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
(global-set-key (kbd "C-\\") 'y:comment-line-or-region)    ; "C-c C-="
(global-set-key (kbd "C-|") 'y:uncomment-line-or-region)  ; "C-c C-+"


;; configure /hl-line-mode/ for /monokai/, enabled in python-mode
    ;; to highlight the single row where the cursor is.
;; configure /highlight-indentation/ for /monokai/, enabled in python-mode
    ;; to highlight indentations
;; NOT work! Having to be and having been set in .emacs
; (cond 
;  ((string-equal 'custom-enabled-themes "ymonokai")
;   ;(global-hl-line-mode t)
;   (set-face-background 'hl-line "gray27") 
;   (set-face-attribute hl-line-face nil :underline nil)
;   (set-face-background 'highlight-indent-face "gray27")
;   ))


;; Toogle window split between horizontal-split and vertical-split
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