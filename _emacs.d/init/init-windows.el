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

;; Much better window operations via Hydra+Ace-window
(when *use-hydra* (require 'init-hydra-aw))

;; ----------------------------------------------
;; numbering windows
;; ----------------------------------------------
(use-package winum
  :demand
  :config
  (setq winum-auto-setup-mode-line nil) ;; avoid duplicate winnum in spaceline
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

;; ---------------------------------------------------------------
;; /Avy/: jump to char/words in tree-style
;; ---------------------------------------------------------------
(use-package avy
  :demand
  :bind (("C-'"     . avy-goto-char)   ;; C-:
         ("M-'"     . avy-goto-char-2) ;; C-'
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)
         ("M-g w"   . avy-goto-word-1) ;; avy-goto-word-0: too many candiates
         ("M-g M-r" . avy-resume))
  :config
  (avy-setup-default))

;; ------------------------------------------------
;; Directory explorer (regular, /dired/)
;; ------------------------------------------------
(require 'dired-x)
(setq dired-omit-files
      "^\\.?#\\|^#.*\\|\\.DS_Store$\\|^Icon.*\\|\\..*\\.cache$\\|\\.git$\\|\\.dropbox\\|\\.directory\\|.*\\.synctex.gz$\\|.*\\.out$\\|.*\\.fdb_latexmk\\|.*\\.fls\\|.*\\.ilg\\|.*\\.ind\\|.*\\.nlo\\|.*\\.nls")
(delete ".bbl" dired-omit-extensions)
(add-hook 'dired-mode-hook (lambda() (dired-omit-mode 1)))
;; suppress errors due to no support of "ls --dired" on osx
(when (string= system-type "darwin") (setq dired-use-ls-dired nil))

;; ------------------------------------------------
;; Directory explorers (tree)
;; ------------------------------------------------
(require 'init-treemacs)

;; ------------------------------------------------
;; /ibuffer/: manage opened buffers
;; ------------------------------------------------
(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon
              my-ibuffer-find-file)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Grouping buffers (overwritten if using /ibuffer-projectile/)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Dired" (mode . dired-mode))
                 ("Org"   (name . "^.*org$"))
                 ("Writing" (or (mode . markdown-mode)
                                (mode . latex-mode)
                                (name . "^.*txt$")))
                 ("Shell" (or (mode . eshell-mode)
                              (name . "^\\*R\\*$")
                              (name . "^\\*ielm\\*$")
                              (mode . inferior-python-mode)))
                 ("Programming" (or (mode . scheme-mode)
                                    (mode . python-mode)
                                    (mode . ess-mode)
                                    (mode . sh-mode)
                                    (mode . matlab-mode)
                                    (mode . c-mode)
                                    (mode . c++-mode)
                                    (mode . cmake-mode)
                                    (name . ".dir-locals.el")
                                    (name . ".gitignore")
                                    (name . ".projectile")
                                    (name . "[mM]akefile")))
                 ("Web" (or (mode . html-mode)
                            (mode . css-mode)
                            (mode . json-mode)
                            (mode . js2-mode)))
                 ("Notebook" (or (mode . easy-hugo-mode)
                                 (mode . deft-mode)))
                 ("Emacs" (or (mode . emacs-lisp-mode)
                              (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$")
                              (name . "^\\*dashboard\\*$")))
                 ("LSP"   (or (name . "^\\*lsp-log\\*$")
                              (name . "^\\*mspyls\\*$")
                              (name . "^\\*mspyls::stderr\\*$")
                              (name . "^\\*pyls\\*$")
                              (name . "^\\*clangd\\*$")
                              (name . "^\\*clangd::stderr\\*$")
                              ))
                 ("Debugging" (or (mode . dap-server-log-mode)
                                  (name . "^debug.el$")
                                  (name . "^\\*dap-ui-breakpoints\\*$")
                                  (name . "^\\*dap-ui-locals\\*$")
                                  (name . "^\\*dap-ui-expressions\\*$")
                                  (name . "^\\*dap-ui-sessions\\*$")
                                  (name . "^\\*dap-ui-repl\\*$")))
                 ("Terms" (or (mode . term-mode)
                              (mode . ansi-term-mode)))
                 ("misc." (or (name . "^\\*Help\\*$")
                                   (name . "^\\*Warnings\\*$")
                                   (name . "^\\*Compile-Log*\\*$")
                                   (mode . TeX-output-mode)
                                   (mode . reftex-toc-mode)
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

  ;; Display buffer icons on GUI
  (when (and *is-graphic*
             (require 'all-the-icons nil t))
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :disabled
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.2)
               " ")
            "Project: "))))

;; ------------------------------------------------
;; /imenu-list/: show imenu entries in side bar
;; ------------------------------------------------
;; Integrations:
;;   - /markdown-mode/ to show headings
(use-package imenu-list
  :bind ("C-x C-'" . imenu-list-smart-toggle)
  :config
  ;; (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t))

;; ----------------------------------------------
;; /deft/: Organise and browse notes
;; ----------------------------------------------
(use-package deft
  :commands (deft)
  :bind (("C-x f" . deft-find-file))
  :config
  (setq deft-extensions '("org" "md")
        deft-default-extension "org"
        deft-directory  "~/Public/Dropbox/oracleyue/OrgNote")
  (setq deft-recursive t))

;; ------------------------------------------------
;; All-the-icons supports for Dired
;; ------------------------------------------------
(when *enable-all-the-icons*
  (use-package all-the-icons-dired
    :diminish
    :functions (dired-move-to-filename
                dired-get-filename
                my-all-the-icons-dired--display)
    :commands all-the-icons-dired--display
    :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (declare-function all-the-icons-octicon 'all-the-icons)
    (declare-function all-the-icons-match-to-alist 'all-the-icons)
    (declare-function all-the-icons-dir-is-submodule 'all-the-icons)
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert " ")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (file-local-name (dired-get-filename nil t))))
                      (if (file-directory-p filename)
                          (let ((icon (cond
                                       (remote-p
                                        (all-the-icons-octicon "file-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-symlink-p filename)
                                        (all-the-icons-octicon "file-symlink-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((all-the-icons-dir-is-submodule filename)
                                        (all-the-icons-octicon "file-submodule"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-exists-p (format "%s/.git" filename))
                                        (all-the-icons-octicon "repo"
                                                               :height 1.1
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       (t (let ((matcher (all-the-icons-match-to-alist
                                                          file all-the-icons-dir-icon-alist)))
                                            (apply (car matcher)
                                                   (list (cadr matcher)
                                                         :face 'all-the-icons-dired-dir-face
                                                         :v-adjust all-the-icons-dired-v-adjust)))))))
                            (insert icon))
                        (insert (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
                    (insert "\t "))))   ; Align and keep one space for refeshing after operations
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))
  )


(provide 'init-windows)
;; ================================================
;; init-windows.el ends here
