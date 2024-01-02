;; ================================================================
;; Modes to support snippets
;; ================================================================
;; Last modified on 12 Aug 2022

;; /Yasnippet/ A template system
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  ;; suppress warning
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

;; (use-package yasnippet-snippets
;;   :after yasnippet)


(provide 'init-snippets)
;; ================================================
;; init-snippets.el ends here
