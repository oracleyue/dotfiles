;; Notes: having been modified by oracleyue.
;; Last modified on 24 Aug 2017


;; Ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'smart-operator)

;; Open Next Line
(require 'open-next-line)

;; Code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; duplicate a line
(define-key python-mode-map (kbd "C-c y") 'djcb-duplicate-line) ;oracleyue

;; duplicate a line and comment the first
(define-key python-mode-map (kbd "C-c c")
  (lambda()  (interactive)(djcb-duplicate-line t)))  ;oracleyue

;; Code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-left start end))
  (setq deactivate-mark nil)
)

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-right start end))
  (setq deactivate-mark nil)
)

;;  Evaluate a python block contributed by eepgwde
(defun python-shell-send-block (arg)
  "Send the current block to inferior Python process."
  (interactive "P")
  (python-shell-send-region
   (progn
     (progn (beginning-of-line) (point-marker)))
   (progn
     (progn (forward-paragraph) (point-marker)))))


;; Defining some useful keybindings
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "M-<right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "M-<left>")
	      'balle-python-shift-left)
	    (define-key python-mode-map (kbd "C-c C-b")
	      'python-shell-send-block)))


;; Other useful stuff

; delete seleted text when typing
(delete-selection-mode 1)

; highlight brackets
;(show-paren-mode t)

; highlight current line & highlight indentation
(defun epy-edit-hl-config()
  ;; highlight indentation
  (require 'highlight-indentation)
  (highlight-indentation)
  ;; highlight current line
  (hl-line-mode t))
;; (add-hook 'python-mode-hook 'epy-edit-hl-config) ;; enabled in "init-progtools.el"



(provide 'epy-editing)
