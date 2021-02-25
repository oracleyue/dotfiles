;; ===============================================================
;; General Plain Text Supports
;; ===============================================================
;; Last modified on 20 Feb 2020


;; ---------------------------------------------
;; Basics of text-mode
;; ---------------------------------------------
(setq-default major-mode 'text-mode)

;; line wrapping
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; spell checking
;; auto-correct words: "C-." or "C-M-i"
(use-package flyspell
  :ensure nil
  :demand
  :diminish
  :hook (text-mode . flyspell-mode)
  :init (setq ispell-dictionary "british")
  :config
  (defun zyue/toggle-dictionary ()
    "Toggle flyspell dictionary between the American and the British."
    (interactive)
    (if (string-equal ispell-dictionary "british")
        (setq ispell-dictionary "american")
      (setq ispell-dictionary "british"))
    (ispell-kill-ispell t)
    (message "%s" ispell-dictionary)))

;; ---------------------------------------------
;; /Grammarly/ for English checking
;; ---------------------------------------------
(use-package grammarly  ;; Grammarly API interface
  :disabled
  :config
  (use-package flycheck
    :diminish
    :hook ((text-mode   . flycheck-mode)
           (LaTeX-mode  . flycheck-mode))
    :config
    (add-hook 'org-mode-hook (lambda () (flycheck-mode -1))))
  ;; flycheck interface for Grammarly
  ;; grammarly available for: text, latex, org, markdown
  (use-package flycheck-grammarly :demand))



(provide 'init-text)
;; ================================================
;; init-text.el ends here
