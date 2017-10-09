;;
;; Use /smart-mode-line/
;;
(defun y:use-smart-mode-line ()
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'powerline)
  (sml/setup))


;;
;; User customized mode-line
;;
(defun y:customize-mode-line ()
  (if (eq 'monokai (car custom-enabled-themes))
      (progn
        ;; define the colors only for the monokai theme
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

        ;; set default mode line for monokai theme
        (unless window-system        ;; in terminal
          (set-face-attribute 'mode-line nil
                              :background "color-238"))
        (setq-default mode-line-format
                      (list
                       ;; default part
                       "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification

                       ;; the buffer name; the file name as a tool tip
                       ;;   (default: 'face 'font-lock-keyword-face)
                       '(:eval (propertize "%b " 'face 'mode-line-buffer-name-face-y
                                           'help-echo (buffer-file-name)))

                       ;; line and column (default: 'face 'font-lock-type-face)
                       "(" ;; '%02' to set to 2 chars at least; prevents flickering
                       (propertize "%02l" 'face 'mode-line-linum-face-y) ","
                       (propertize "%02c")
                       ") "

                       ;; relative position, size of file
                       ;;   (default: 'face 'font-lock-constant-face)
                       "["
                       (propertize "%p") ;; % above top
                       "/"
                       (propertize "%I") ;; size
                       "] "

                       ;; the current major mode for the buffer
                       ;;  (default: 'face 'font-lock-string-face)
                       "["
                       '(:eval (propertize (if (listp mode-name)
                                               (mapconcat 'identity (cdr mode-name) "/")
                                             mode-name)
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
                       "  ---"
                       '(:eval (propertize (format-time-string "%H:%M")
                                           'help-echo
                                           (concat (format-time-string "%c; ")
                                                   (emacs-uptime "Uptime:%hh"))))
                       "---"

                       ;; i don't want to see minor-modes; but if you want, uncomment this:
                       ;; minor-mode-alist  ;; list of minor modes

                       ;; "%-" ;; fill with '-'
                       )))
    ;; set default mode line for other themes
    (set-face-attribute 'mode-line nil
                        :box '(:line-width 1 :style released-button))
    (setq-default mode-line-format
                  (list
                   ;; default part
                   "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification

                   ;; the buffer name; the file name as a tool tip
                   '(:eval (propertize "%b " 'face 'bold
                                       'help-echo (buffer-file-name)))

                   ;; line and column
                   "("
                   (propertize "%02l") ","
                   (propertize "%02c")
                   ") "

                   ;; relative position, size of file
                   "["
                   (propertize "%p") ;; % above top
                   "/"
                   (propertize "%I") ;; size
                   "] "

                   ;; the current major mode for the buffer
                   "["
                   '(:eval (propertize (if (listp mode-name)
                                           (mapconcat 'identity (cdr mode-name) "/")
                                         mode-name)
                                       'help-echo buffer-file-coding-system))
                   "] "
                   ;; "      "
                   "[" ;; insert vs overwrite mode, input-method in a tooltip
                   '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                       'face
                                       (if overwrite-mode 'font-lock-warning-face nil)
                                       'help-echo (concat "Buffer is in "
                                                          (if overwrite-mode "overwrite" "insert") " mode")))

                   ;; was this buffer modified since the last save?
                   '(:eval (when (buffer-modified-p)
                             (concat ","  (propertize "Mod" 'face 'error
                                                      'help-echo "Buffer has been modified"))))

                   ;; is this buffer read-only?
                   '(:eval (when buffer-read-only
                             (concat ","  (propertize "RO" 'face 'font-lock-type-face
                                                      'help-echo "Buffer is read-only"))))
                   "] "

                   ;; add the time, with the date and the emacs uptime in the tooltip
                   "  ---"
                   '(:eval (propertize (format-time-string "%H:%M")
                                       'help-echo
                                       (concat (format-time-string "%c; ")
                                               (emacs-uptime "Uptime:%hh"))))
                   "---"
                   ))))

;;
;; Interface
;;
(defun y:setup-mode-line ()
  (y:use-smart-mode-line)
  ;; (y:customize-mode-line)
  )



(provide 'customize-modeline)
;; ================================================
;; customize-modeline.el ends here