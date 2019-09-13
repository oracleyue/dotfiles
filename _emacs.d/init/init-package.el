;; ------------------------------------------------
;; Emacs package management system
;; ------------------------------------------------

;; packages path using /homebrew/
(when (string-equal system-type "darwin")
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; setup package archives
(require 'package)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org"   . "http://orgmode.org/elpa/")))

;; initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; setup /use-package/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; set before loading /use-package/
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

;; ------------------------------------------------
;; Customized functions for package installation
;; ------------------------------------------------

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

;; To install packages, call
;; (custom/install-packages YOUR_PACKAGE_LIST)

;; ------------------------------------------------
;; Functions for elisp compilation
;; ------------------------------------------------

;; Easy compiling .el files
(defun y/byte-compile-directory (directory)
  "Byte-compile all *.el in the directory if the corresponding
.elc doesn't exist."

  (setq flist (directory-files directory nil ".*\.el$"))
  (dolist (fname flist)
    (unless (file-exists-p
             (concat (expand-file-name (file-name-base fname)
                                       directory) ".elc"))
      (byte-compile-file (expand-file-name fname directory)))))

;; ------------------------------------------------
;; Functions for init config in org-mode
;; ------------------------------------------------

;; extracting .el from .org config files
(require 'ob-tangle)
(defun zyue/init-org2el-all ()
  "Use org-babel to extract elisp code blocks from all .org files
in ~/.emacs.d/init/ and export them using the same file names."
  (interactive)
  (setq prepath (concat user-emacs-directory "init/"))
  (let ((files (directory-files prepath nil "\\.org$")))
    (dolist (file files)
      (setq file-dest (concat prepath
                              (file-name-sans-extension file) ".el"))
      (setq file (concat  prepath file))
      (org-babel-tangle-file file file-dest "emacs-lisp"))))

(defun zyue/init-org2el ()
  "Use org-babel to extract elisp code blocks from the current .org
file and export into ~/.emacs.d/init/ with the same file name."
  (interactive)
  (setq outpath (concat user-emacs-directory "init/"))
  (let ((file (buffer-file-name)))
    (if (not (string-equal (file-name-extension file) "org"))
        (user-error "Error: not an org-mode file")
      (setq file-dest (concat outpath
                              (file-name-base file) ".el"))
      (org-babel-tangle-file file file-dest "emacs-lisp"))))


(provide 'init-package)
;; ================================================
;; init-package.el ends here
