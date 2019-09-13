;; ================================================================
;; Collections of Misc Modes for Assistance
;; ================================================================

;; Install required Emacs packages
;; (setq custom/misc-packages
;;       '(goldendict
;;         ledger-mode))
;; (custom/install-packages custom/misc-packages)


;; /goldendict/: use GoldenDict in Emacs
(use-package goldendict
  :bind ("M-g ." . goldendict-dwim)
  :config
  (when *is-mac*
    (setq goldendict-cmd
          "/Applications/GoldenDict.app/Contents/MacOS/GoldenDict")))

;; /ledger-mode/: financial accounting
(use-package ledger-mode)
;; It also provides Babel in org-mode for ledger src blocks.


(provide 'init-misc)
;; ================================================
;; init-misc.el ends here
