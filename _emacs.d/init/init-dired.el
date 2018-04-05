;; ================================================================
;; Directory Explorers and Window Management
;; ================================================================


;; ----------------------------------------------
;; Basic settings for window management
;; ----------------------------------------------

;; use "super-<left>", "super-<right>" to move between frames

;; use "S-<left>", "S-<right>", "S-<up>", "S-<down>" to move between splitted windows
(windmove-default-keybindings)


;; ----------------------------------------------
;; /golden-ratio/: resize multiple windows
;; ----------------------------------------------
(require 'golden-ratio)
(when (string-equal "main" (daemonp))
  (golden-ratio-mode t))
(eval-after-load "golden-ratio"
  '(progn ;(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
     (add-to-list 'golden-ratio-inhibit-functions 'pl/no-golden-ratio-popwin)
     (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
     (add-to-list 'golden-ratio-exclude-modes "direx:direx-mode")
     (add-to-list 'golden-ratio-exclude-modes "emacs-lisp-mode")
     (add-to-list 'golden-ratio-exclude-modes "c-mode")
     (add-to-list 'golden-ratio-exclude-modes "c++-mode")
     (add-to-list 'golden-ratio-exclude-modes "ess-mode")
     (add-to-list 'golden-ratio-exclude-modes "python-mode")))
(defun pl/helm-alive-p () (and (boundp 'helm-alive-p)
                               (symbol-value 'helm-alive-p)))
(defun pl/no-golden-ratio-popwin ()
  "Disable golden-ratio for popwin buffer."
  (or (pl/no-golden-ratio-for-buffers " *guide-key*")
      (pl/no-golden-ratio-for-buffers " *popwin-dummy*")
      (pl/no-golden-ratio-for-buffers "*Ilist*")))
(defun pl/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))


;; ------------------------------------------------
;; /popwin/: manage popup (temporary) buffers
;; ------------------------------------------------
(require 'popwin)
(popwin-mode 1)


;; ------------------------------------------------
;; Directory explorer (regular, /dired/)
;; ------------------------------------------------
(require 'dired-x)
(setq dired-omit-files
      "^\\.?#\\|^#.*\\|\\.DS_Store$\\|^Icon.*\\|\\..*\\.cache$\\|\\.git\\|\\.dropbox\\|\\.directory\\|.*\\.synctex.gz$\\|.*\\.out$\\|.*\\.fdb_latexmk\\|.*\\.fls\\|.*\\.ilg\\|.*\\.ind\\|.*\\.nlo\\|.*\\.nls")
(delete ".bbl" dired-omit-extensions)
(add-hook 'dired-mode-hook (lambda() (dired-omit-mode 1)))
;; suppress errors due to no support of "ls --dired" on osx
(when (string= system-type "darwin") (setq dired-use-ls-dired nil))


;; ------------------------------------------------
;; Directory explorers (tree)
;; ------------------------------------------------

;; directory explorer in tree: /direx/ (frontend for /jedi-direx/)
(when (string-equal y:tree-manager "direx")
  (require 'direx)
  (push '(direx:direx-mode :position left :width 27 :dedicated t)
        popwin:special-display-config)
  (define-key direx:direx-mode-map (kbd "<tab>") 'direx:toggle-item) ;; fix tab
  (define-key direx:direx-mode-map (kbd "x") 'direx/y:kill-buffer)
  (setq direx:leaf-icon "  "
        direx:open-icon "▾ "
        direx:closed-icon "▸ ")
  (defun direx/y:kill-buffer (&optional item) (interactive)
         (kill-buffer (current-buffer)))
  (unless (cdr (assoc "neotree" y:use-direx-or-neotree))
    (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)))

;; directory explorer in tree: /neotree/
(when (string-equal y:tree-manager "neotree")
  (require 'neotree)
  (setq neo-theme 'arrow)
  (global-set-key (kbd "C-x C-j") 'neotree-toggle)
  (define-key neotree-mode-map (kbd "i") 'neotree-enter)
  (define-key neotree-mode-map (kbd "<tab>") 'neotree-enter) ;;fix tab
  (setq neo-show-hidden-files nil)
  (eval-after-load "neotree"      ;; toggle by "H" in neotree
    '(setq neo-hidden-regexp-list '("^\\..*" "^#.*" "^Icon.*" ".DS_Store" ".dropbox" ".*~"))))


;; ------------------------------------------------
;; /ibuffer/: manage opened buffers
;; ------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org"   (name . "^.*org$"))
               ("writing" (or (mode . text-mode)
                              (mode . markdown-mode)
                              (mode . latex-mode)))
               ("shell" (or (mode . eshell-mode)
                            (name . "^\\*R\\*$")
                            (name . "^\\*ielm\\*$")
                            (mode . inferior-python-mode)))
               ("programming" (or (mode . scheme-mode)
                                  (mode . python-mode)
                                  (mode . ess-mode)
                                  (mode . sh-mode)
                                  (mode . matlab-mode)
                                  (mode . c-mode)
                                  (mode . c++-mode)
                                  (mode . cmake-mode)
                                  (name . ".dir-locals.el")
                                  (name . "[mM]akefile")))
               ("web" (or (mode . html-mode)
                          (mode . css-mode)
                          (mode . json-mode)
                          (mode . js2-mode)))
               ("blog" (mode . hexo-mode))
               ("emacs" (or (mode . emacs-lisp-mode)
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))
               ("tags viewer" (mode . direx:direx-mode))
               ("miscellany" (or (name . "^\\*Help\\*$")
                                 (name . "^\\*Warnings\\*$")
                                 (name . "clang-complete")
                                 (name . "[hH]elm.*")
                                 (mode . TeX-output-mode)
                                 (mode . reftex-toc-mode)
                                 (mode . compilation-mode)))
               ))))
(add-hook 'ibuffer-mode-hook (lambda ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show certain buffers
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*epc con .*\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*ESS\\*")

;; don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)


;; ------------------------------------------------
;; /imenu-list/: show imenu entries in side bar
;; ------------------------------------------------
;; Integrations:
;;   - /markdown-mode/ to show headings

(require 'imenu-list)
(global-set-key (kbd "C-x C-'") #'imenu-list-smart-toggle)
;; (setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)


; ------------------------------------------------
;; /TRAMP/: manage ssh and remote access
; ------------------------------------------------
(setq tramp-default-method "ssh")
;; usages:
;; - "C-x C-f /ssh:gaia:/home/users/zuogong.yue/..." or without "ssh:"
;; - "C-x C-f /sudo::/etc/hosts"


;; ----------------------------------------------
;; /engine-mode/: manage web search
;; ----------------------------------------------
(require 'engine-mode)
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
  "https://scholar.google.lu/scholar?hl=en&q=%s"
  :keybinding "s")
(defengine stack-overflow "https://stackoverflow.com/search?q=%s"
  :keybinding "o")



(provide 'init-dired)
;; ================================================
;; init-dired.el ends here
