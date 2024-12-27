;; ================================================================
;; Directory Explorers and Window Management
;; ================================================================
;; Last modified on 18 Jan 2019

;; ----------------------------------------------
;; Basic settings for window management
;; ----------------------------------------------
;; use "super-<left>", "super-<right>" to move between frames
;; use "S-<left>", "S-<right>", "S-<up>", "S-<down>" to move between windows
;; (windmove-default-keybindings)  ;; enable

;; alternative: better with /ace-window/+/hydra/ in "init-hydra.el"

;; Emacs in console or tty:
;; use "F10" to open menu bar or "M-`" to choose menu items in minibuffer


;; ----------------------------------------------
;; numbering windows
;; ----------------------------------------------
(use-package winum
  :demand
  :config
  ;; (setq winum-auto-setup-mode-line nil) ;; avoid duplicate winnum in spaceline
  (winum-mode)
  :bind (("s-1" . winum-select-window-1)
         ("s-2" . winum-select-window-2)
         ("s-3" . winum-select-window-3)
         ("s-4" . winum-select-window-4)
         ("s-5" . winum-select-window-5)
         ("s-6" . winum-select-window-6)
         ("s-7" . winum-select-window-7)
         ("s-8" . winum-select-window-8)
         ("s-9" . winum-select-window-9)
         ("s-0" . winum-select-window-10)))
;; =C-x w <n>=: select window <n>, where <n> ranges from 0 to 9
;; =C-x w `=: select window by number, which is inserted in minibuffer

;; ------------------------------------------------
;; Directory explorer (regular, /dired/)
;; ------------------------------------------------
;; basic functions
(setq dired-dwim-target         t
      dired-recursive-copies    t
      dired-recursive-copies    'always
      dired-recursive-deletes   t)

;; suppress errors due to no support of "ls --dired" on osx
;; (when (string= system-type "darwin") (setq dired-use-ls-dired nil))

;; hide file property columns in Dired
(when (string= system-type "darwin")  ;; use "gls" for better supports
  (setq dired-use-ls-dired t)
  (setq insert-directory-program "/usr/local/bin/gls"))
;; hide owner/group information via "ls" options (if more, turn on hide-details mode)
(setq dired-listing-switches "-algGhv --group-directories-first")
;; use "dired-hide-details-mode" to hide all (NOTE: use key "(" to toggle hiding)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; omitting special files
(require 'dired-x)
(setq dired-omit-files
      "^\\.?#\\|^#.*\\|\\.DS_Store$\\|^Icon.*\\|\\..*\\.cache$\\|\\.git$")
;; apps aux files
(setq dired-omit-files
      (concat dired-omit-files
              "\\|\\.obsidian$\\|\\.dropbox$\\|\\.directory$\\|\\.fdignore$"))
;; LaTeX aux files
(setq dired-omit-files
      (concat dired-omit-files
              "\\|.*\\.synctex.gz$\\|.*\\.out$\\|.*\\.fdb_latexmk\\|.*\\.fls\\|.*\\.ilg\\|.*\\.ind\\|.*\\.nlo\\|.*\\.nls"))
(delete ".bbl" dired-omit-extensions)
;; bug: turn on in hook fails dired open directories in emacs@30
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; Icons supports for Dired
(if (string= *icons-type* "nerd-icons")
    (use-package nerd-icons-dired
      :demand
      :hook (dired-mode . nerd-icons-dired-mode))
  ;; use all-the-icons support
  (use-package all-the-icons-dired
    :demand
    :hook (dired-mode . all-the-icons-dired-mode)))

;; ------------------------------------------------
;; /ibuffer/: manage opened buffers
;; ------------------------------------------------
(use-package ibuffer
  :ensure nil
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Grouping buffers (overwritten if using /ibuffer-projectile/)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Explorer" (mode . dired-mode))
                 ("Home"    (or (name . "^\\*dashboard*\\*$")
                                (name . "^\\*scratch\\*$")))
                 ("Org"      (name . "^.*org$"))
                 ("Writing" (or (mode . markdown-mode)
                                (mode . latex-mode)
                                (name . "^.*txt$")))
                 ("Blog/RSS" (or (mode . easy-hugo-mode)
                                 ;; (mode . deft-mode)
                                 (mode . elfeed-mode)
                                 (mode . elfeed-search-mode)
                                 (mode . elfeed-show-mode)))
                 ("Programming" (or (mode . scheme-mode)
                                    (mode . python-mode)
                                    (mode . ess-mode)
                                    (mode . sh-mode)
                                    (mode . octave-mode)
                                    (mode . c-mode)
                                    (mode . c++-mode)
                                    (mode . cmake-mode)
                                    (mode . html-mode)
                                    (mode . css-mode)
                                    (mode . json-mode)
                                    (mode . js2-mode)
                                    (name . ".dir-locals.el")
                                    (name . ".gitignore")
                                    (name . ".projectile")
                                    (name . "[mM]akefile")))
                 ("Git" (or (mode . magit-mode)
                            (mode . magit-status-mode)
                            (mode . magit-diff-mode)
                            (mode . magit-process-mode)))
                 ("ELisp" (or (name . "^.*\\.el$")
                              (mode . emacs-lisp-mode)))
                 ("LSP"   (or (name . "^\\*lsp-log\\*$")
                              (name . "^\\*mspyls\\*$")
                              (name . "^\\*EGLOT.*\\*$")
                              (name . "^\\*mspyls::stderr\\*$")
                              (name . "^\\*pyls\\*$")
                              (name . "^\\*clangd\\*$")
                              (name . "^\\*clangd::stderr\\*$")))
                 ("Debugging" (or (mode . dap-server-log-mode)
                                  (name . "^debug.el$")
                                  (name . "^\\*dap-ui-breakpoints\\*$")
                                  (name . "^\\*dap-ui-locals\\*$")
                                  (name . "^\\*dap-ui-expressions\\*$")
                                  (name . "^\\*dap-ui-sessions\\*$")
                                  (name . "^\\*dap-ui-repl\\*$")))
                 ("Terms" (or (mode . term-mode)
                              (mode . ansi-term-mode)
                              (mode . eshell-mode)
                              (name . "^\\*R\\*$")
                              (name . "^\\*ielm\\*$")
                              (mode . inferior-python-mode)))
                 ("misc." (or (name . "^\\*Help\\*$")
                              (name . "^\\*Messages\\*$")
                              (name . "^\\*Compile-Log*\\*$")
                              (name . "^\\*osx-dictionary\\*$")
                              (name . "^\\*eldoc\\*$")
                              (mode . compilation-mode)))
                 ))))

  ;; Automatically update ibuffer
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))

  ;; Don't show certain buffers
  (require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*epc con .*\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*ESS\\*")

  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Don't ask for confirmation to delete marked buffers
  (setq ibuffer-expert t)

  ;; Use counsel to find-file
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))

;; Icons support for ibuffer
(if (string= *icons-type* "nerd-icons")
    ;; /nerd-icons/ support
    (use-package nerd-icons-ibuffer
      :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
  ;; /all-the-icons/ support
  (use-package all-the-icons-ibuffer
    :hook (ibuffer-mode . all-the-icons-ibuffer-mode)))

;; ------------------------------------------------
;; /imenu-list/: show imenu entries in side bar
;; ------------------------------------------------
;; Integrations:
;;   - /markdown-mode/ to show headings
(use-package imenu-list
  :bind ("M-g M-i" . imenu-list-smart-toggle)
  :config
  ;; (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t))

;; ------------------------------------------------
;; Hydra with ace-window: better window management
;; ------------------------------------------------
;; Ace window management
(use-package ace-window
  :bind   ("M-j" . ace-window)
  :config (setq aw-scope 'frame))

;; Tranpose window layout in frame
(use-package transpose-frame)

;; Adjust window sizes
(require 'move-border)

;; Hydra integration
(global-set-key (kbd "M-g SPC") 'hydra-jp-window/body)

(defvar jp-window--title
  (pretty-hydra-title "Window Management" 'mdicon "nf-md-dock_window"))
(pretty-hydra-define hydra-jp-window
  (:foreign-keys warn :title jp-window--title :quit-key ("q" "SPC" "C-g"))
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))

   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("b" split-window-right "horizontally")
    ("v" split-window-below "vertically")
    ("t" transpose-frame "transpose layout"))

   "Zoom"
   (("+" text-scale-increase "in")
    ("=" text-scale-increase nil)
    ("-" text-scale-decrease "out")
    ("0" text-scale-adjust "reset"))))


(provide 'init-windows)
;; ================================================
;; init-windows.el ends here
