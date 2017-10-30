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
        auto-complete
        auto-complete-c-headers
        auto-complete-clang
        bash-completion
        company
        company-c-headers
        company-jedi
        company-quickhelp
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
        helm
        helm-core
        helm-gtags
        helm-projectile
        helm-swoop
        iedit
        jedi
        jedi-core
        jedi-direx
        julia-mode
        key-combo
        magit
        markdown-mode
        matlab-mode
        multiple-cursors
        neotree
        popup
        popwin
        pos-tip
        projectile
        smartparens
        srefactor
        stickyfunc-enhance
        undo-tree
        yasnippet
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



(provide 'emacs-init-packages)
;; ================================================
;; emacs-init-packages.el ends here