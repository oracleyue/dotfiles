;; ================================================================
;; Collections of Misc Modes for Assistance
;; ================================================================


;; /command-log/: show key-strokes/commands in demo
(use-package command-log-mode)

;; /goldendict/: use GoldenDict in Emacs
(use-package goldendict
  :bind ("M-g d" . goldendict-dwim)
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
