;; ---------------- emacs package system -------------------
;; packages installed by /homebrew/
(when (string-equal system-type "darwin")
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path)))

;; package management by ELPA
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
;; ---------------------------------------------------------

;; List of Required Packages
(setq custom/packages
      '(auctex
        bash-completion
        csv-mode
        engine-mode
        dash
        direx
        ess
        expand-region
        exec-path-from-shell
        flycheck
        flymake-easy
        flymake-google-cpplint
        function-args
        golden-ratio
        google-c-style
        iedit
        imenu-list
        julia-mode
        key-combo
        magit
        markdown-mode
        matlab-mode
        multiple-cursors
        neotree
        popwin
        smartparens
        srefactor
        stickyfunc-enhance
        undo-tree
        zeal-at-point))

;; Functions to check and install packages
(defun custom/packages-installed-p (pkg-list)
  "Check whether certain packages have not yet been installed."
  (catch 'exit
    (dolist (pkg pkg-list)
      (unless (package-installed-p pkg)
        (throw 'exit nil)))
    (throw 'exit t)))

(defun custom/install-packages (pkg-list)
  "Interface to install essential packages."
  (unless (custom/packages-installed-p pkg-list)
    ;; list pkgs to be installed
    (message "\n%s" "Refreshing package database...")
    (message "----------------------")
    (dolist (pkg pkg-list)
      (unless (package-installed-p pkg)
        (message "+ %s" pkg)))
    (message "----------------------\n")
    ;; install pkgs
    (package-refresh-contents)
    (dolist (pkg pkg-list)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

;; Install packages that have not yet been installed
(custom/install-packages custom/packages)


;; User-defined functions
(defun byte-compile-directory (directory)
  "Byte-compile all *.el in the directory if the corresponding
.elc doesn't exist."

  (setq flist (directory-files directory nil ".*\.el$"))
  (dolist (fname flist)
    (unless (file-exists-p
             (concat (expand-file-name (file-name-base fname)
                                       directory) ".elc"))
      (byte-compile-file (expand-file-name fname directory)))))



(provide 'init-packages)
;; ================================================
;; init-packages.el ends here
