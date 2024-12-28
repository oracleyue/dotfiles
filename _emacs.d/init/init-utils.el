;; ===============================================================
;; Utilities and Apps called by Emacs
;; ===============================================================
;; Last modified on 08 Mar 2021

;; ----------------------------------------------
;; Default web browser
;; ----------------------------------------------
(setq browse-url-browser-function 'browse-url-default-browser)

;; choose safari
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program
;;       (expand-file-name "~/bin/web-browser"))

;; ----------------------------------------------
;; /engine-mode/: manage web search
;; ----------------------------------------------
(use-package engine-mode
  :config
  (engine-mode t)
  (setq engine/browser-function 'browse-url-default-browser)
  ;; (engine/set-keymap-prefix (kbd "C-c s"))  ;; change the defaul "C-x /"
  (defengine duckduckgo "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss2?url=search-alias%%3Daps&field-keywords=%s"
    :keybinding "a")
  (defengine google "http://www.google.lu/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine google-scholar
    "https://scholar.google.com/scholar?hl=en&q=%s"
    :keybinding "s")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s"
    :keybinding "o"))

;; ----------------------------------------------
;; Set external software to open documents
;; ----------------------------------------------
(use-package openwith
  :demand
  :config
  (openwith-mode t)
  (if *is-mac*
      (setq openwith-associations '(("\\.pdf\\'" "/usr/bin/open" (file))))
    (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))))))

;; ----------------------------------------------
;; Open folder in Finder
;; ----------------------------------------------
;; (use-package reveal-in-osx-finder
;;   :demand
;;   :if *is-mac*
;;   :after (ivy dired)
;;   :config
;;   (ivy-add-actions 'counsel-find-file
;;                    '(("F" (lambda (str) (reveal-in-osx-finder-as str nil))
;;                       "open in Finder")))
;;   (define-key dired-mode-map "F" 'reveal-in-osx-finder))

;; ----------------------------------------------
;; Apple dictionary in Emacs (show in mini-buffer)
;; ----------------------------------------------
(use-package osx-dictionary
  :if *is-mac*
  :bind (("M-g d" . osx-dictionary-search-word-at-point)))

;; ----------------------------------------------
;; Goldendict in Emacs (show in external app)
;; ----------------------------------------------
(use-package goldendict
  :bind ("M-g D" . goldendict-dwim)
  :config
  (when *is-mac*
    (setq goldendict-cmd
          "/Applications/GoldenDict.app/Contents/MacOS/GoldenDict")))

;; ----------------------------------------------
;; /Go-Translate/ for Google Translate interface
;; ----------------------------------------------
(use-package go-translate
  :ensure nil
  :load-path "site-lisp/go-translate/"
  :bind (("C-c t" . go-translate)
         ("C-c T" . go-translate-popup))
  :config
  (setq go-translate-base-url "https://translate.google.cn"
        go-translate-local-language "fr"
        go-translate-target-language "en")
  (setq go-translate-extra-directions
        '(("de" . "en")
          ("jp" . "zh-CN")
          ("ru" . "zh-CN")))
  ;; fix token-key issue mismatch
  (setq go-translate-token-current (cons 430675 2721866130)))
;; (in minibuffer)
;;  - "C-n" and "C-p" goes to next or prev. language for search
;;  - "Enter" or "C-Enter" in minibuffer to translate (the latter jumps to the result win)
;;  - demo: "C-c t" -> "C-return" -> "C-x C-x" to select -> "M-w" -> "q" -> "C-y"
;; (in result buffer)
;;  - "g": refresh; "q": quit
;;  - "x": exchange source and target and search again
;;  - "M-n" and "M-p" goes to next (source-target) lang for search

;; ----------------------------------------------
;; Managing Ebooks via Calibre within Emacs
;; ----------------------------------------------
;; (require 'init-calibre)


(provide 'init-utils)
;; ================================================
;; init-utils.el ends here
