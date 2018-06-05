;; ================================================================
;; Collections of Misc Modes for Assistance
;; ================================================================

;;
;; /goldendict/: use GoldenDict in Emacs
;;

(use-package goldendict
  :ensure t
  :bind ("M-g ." . goldendict-dwim)
  :config
  (when *is-mac*
    (setq goldendict-cmd
          "/Applications/GoldenDict.app/Contents/MacOS/GoldenDict")))