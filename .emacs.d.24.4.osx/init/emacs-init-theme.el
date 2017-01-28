;;; Set Theme

;; 1) using default theme
;(load-theme 'deeper-blue t)

;; 2) using user-defined theme  /github/: Ethan Schoonover's solarized theme
;; To use dark theme and enabled in terminal, use "set-variable" in .emacs
;;(setq frame-background-mode (quote dark))
;;!! (setq solarized-termcolors 256)   ;distort colors; do NOT enable
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-theme")
;(load-theme 'solarized t)

;; 3) using oracleyue's theme
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'ymonokai t)


;;; Set Frinage 
;; fringe setting (right-only); bug: cause linum-mode to destory the auto-complete popup menu
;(fringe-mode '(0 . nil))


;;; Customize mode-line (not work well for HTML mode)
(defun y:custom-mode-line ()
  (make-face 'mode-line-linum-face-y)
  (make-face 'mode-line-buffer-name-face-y)
  (make-face 'mode-line-plain-face-y)
  ;; define the colors only for the monokai theme
  (if (eq 'ymonokai (car custom-enabled-themes))    
      (progn (set-face-attribute 'mode-line-linum-face-y nil
                                 :foreground "#66D9EF")
             (set-face-attribute 'mode-line-buffer-name-face-y nil
                                 :foreground "#A6E22E"
                                 :bold t)
             (set-face-attribute 'mode-line-plain-face-y nil
                                 :foreground "#F8F8F2")))
  ;; use setq-default to set it for /all/ modes
  (setq-default mode-line-format
                (list
                 ;; " "
                 ;; default part
                 "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification
                                        ;mode-line-buffer-identification
                 ;; "  "
                 ;; the buffer name; the file name as a tool tip (default: 'face 'font-lock-keyword-face)
                 '(:eval (propertize "%b " 'face 'mode-line-buffer-name-face-y
                                     'help-echo (buffer-file-name)))
                 ;; "  "
                 ;; line and column (default: 'face 'font-lock-type-face)
                 "(" ;; '%02' to set to 2 chars at least; prevents flickering
                 (propertize "%02l" 'face 'mode-line-linum-face-y) ","
                 (propertize "%02c") 
                 ") "
                 ;; "  "
                 ;; relative position, size of file (default: 'face 'font-lock-constant-face)
                 "["
                 (propertize "%p") ;; % above top
                 "/"
                 (propertize "%I") ;; size
                 "] "
                 ;; "         "
                 ;; the current major mode for the buffer (default: 'face 'font-lock-string-face)
                 "["

                 '(:eval (propertize "%m"
                                     'help-echo buffer-file-coding-system))
                 "] "
                 ;; "      "
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
                 ;; "                     "
                 ;; add the time, with the date and the emacs uptime in the tooltip
                 "  ---"
                 '(:eval (propertize (format-time-string "%H:%M")
                                     'help-echo
                                     (concat (format-time-string "%c; ")
                                             (emacs-uptime "Uptime:%hh"))))
                 "---"
                 
                 ;; i don't want to see minor-modes; but if you want, uncomment this:
                 ;; minor-mode-alist  ;; list of minor modes
        
                 ;"%-" ;; fill with '-'
                 ))
  )
(y:custom-mode-line)  ;; enable custimization
