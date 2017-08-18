;; List of Required Packages
(defvar custom/packages
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
    undo-tree
    xcscope
    yasnippet
    zeal-at-point) "Default packages")


;; Function to Install Packages
(require 'cl)  ;; common-lisp functions used below
(defun custom/packages-installed-p ()
  (loop for pkg in custom/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

;; Installation
(unless (custom/packages-installed-p)
  (when (not package-archive-contents)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (dolist (pkg custom/packages)
      (when (not (package-installed-p pkg))
        (package-install pkg)))))