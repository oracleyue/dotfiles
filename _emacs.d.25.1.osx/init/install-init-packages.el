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
        dash
        emmet-mode
        ess
        expand-region
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
        js2-mode
        julia-mode
        key-combo
        magit
        markdown-mode
        matlab-mode
        multiple-cursors
        popup
        pos-tip
        projectile
        smartparens
        stickyfunc-enhance
        undo-tree
        xcscope
        yasnippet
        zeal-at-point))


;; Function to Install Packages
(defun custom/packages-installed-p ()
  (catch 'exit
    (dolist (pkg custom/packages)
      (unless (package-installed-p pkg)
        (throw 'exit nil)))
    (throw 'exit t)))

;; Perform Installation
(unless (custom/packages-installed-p)
  ;; list pkgs to be installed
  (message "\n%s" "Refreshing package database...")
  (message "----------------------")
  (dolist (pkg custom/packages)
    (unless (package-installed-p pkg)
      (message "+ %s" pkg)))
  (message "----------------------\n")
  ;; install pkgs
  (package-refresh-contents)
  (dolist (pkg custom/packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))
