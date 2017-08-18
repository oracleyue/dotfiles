; =======================================
;; Project Management for Programming
;;; using /eproject/
    ;(add-to-list 'load-path "~/.emacs.d/git/emacs-for-python/extensions/eproject")
    ;(require 'eproject)
    ;; Disable automatic addition/removal of files from projects. Optional.
    ;(setq prj-autotracking nil)

;;; using /projectile/
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
;; use =C-c p i= to refresh the whole cache

;;; using /helm-projectile/
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)


;; List of most used commands
;; ----------------
;; - helm-projectile-switch-project =C-c p p=
;; - helm-projectile-find-file =C-c p f=
;; - helm-projectile-find-file-dwim =C-c p g=
;; - helm-projectile-find-other-file =C-c p a=
;; - Virtual directory manager
;;   - create a Dired buffer of project files: =C-c f=
;;   - add more files to the Dired buffer: =C-c a=
;; - helm-projectile-ack =C-c p s a=

;; All-in-one command =helm-projectile=
;; - keybinding: C-c p h
;;   + helm-projectile-switch-to-buffer
;;   + helm-projectile-find-file: =C-c p f=
;;   + helm-projectile-switch-project

;; Enter project portal =helm-projectile-switch-project=
;; - keybinding: C-c p p
(setq projectile-switch-project-action 'helm-projectile)
  ;; open Dired in project's directory:      =C-d=
  ;; open project root in vc-dir or magit:   =M-g=
  ;; switch to Esell; open a project Eshell: =M-e=
  ;; grep in projects (prefix =C-u= to recursive grep):  =C-s=
  ;; compile project, run =compile= at the project root: =C-c=
  ;; remove projects (delete marked projects from the list of known projects): =M-D=

;; File management
;; - command: helm-projectile-find-file
;; - keybinding: C-c p f
;;; Open
;; - Find file: =RET= to open files; =M-SPC= to mark files; =M-a= to mark all
;; - Find file other window: =C-c o=
;; - Find file as root: =C-c r=
;;; Move and Reanme
;; - Rename files: =M-R=; =M-SPC= to mark files
;;; Copy and Delete
;; - Copy files: =M-C=
;; - Delete files: =M-D= or =C-c d=
;;; Search and Replace
;; - Grep files: =C-s= (add prefix =C-u= for resursive grep)
;; - Zgrep: =M-g z= (add prefix =C-u= for resursive grep), invoking grep on cpmressed files
;; - Locate (using Unix ~locate~): =C-x C-f= (add =C-u= to specify locate db)
;;; Miscellanies
;; - Insert as org link (C-c @): Insert the current file that highlighted as an Org link. 
;; - Ediff files: =C-==
;; - Ediff Merge files: =C-c ==, when exactly two files are selected
;; - Etags: =M-.=, invoking Etags using helm
;; - Switch to Eshell: =M-e=
;; - Eshell command on files: =M-!=
;; - Symlink files: =M-S=, using absolute path
;; - Relsymlink files: using relative path
;; - Hardlink files: =M-H=
;; - Checksum file: generate file checksum and insert checksum in ~kill-ring~
;; - Print file: =C-c p= (add =C-u= to refresh)

;; Command: helm-projectile-find-file-in-known-projects
;; - keybinding: C-c p F

;; Command: helm-projectile-find-file-dwim
;; - keybinding: C-c p g

;; Command: helm-projectile-find-dir
;; - keybinding: C-c p d
;; + open Dired in project's directory
;; + switch to Eshell: =M-e=
;; + grep in projects: =C-s= (add prefix =C-u= for recurse grep)

;; Command: helm-projectile-recentf
;; - keybinding: C-c p e

;; Command: helm-projectile-find-other-file
;; - keybinding: C-c p a
;; - usage: switch between files with the same name but different extensions.
;;          With prefix argument =C-u=, enable flex-matching that match any file that contains contains the name of current file.
;; - variable: projectile-other-file-alist
;; adding the switch between html <-> js
(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html

;; Caching
;; - command: projectile-invalidate-cache =C-c p i=
;; - command: projectile-cache-current-file =C-c p z=
;; - command: projectile-purge-file-from-cache
;; - command: projectile-purge-dir-from-cache

;; Virtual directory manager
;; when in a *helm-projectile-find-file* session:
;; - create Dired buffer from files: =C-c f=
;; - add files to Dired buffer: =C-c a=
;; - remove entry from from Dired buffer: =C-c d=

;; Buffer management
;; - command: helm-projectile-switch-project =C-c p b=

;; Search in project
;; --------
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "run")
     (add-to-list 'grep-find-ignored-directories "obj")))
;(add-to-list 'projectile-globally-ignored-files "run")
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
;(add-to-list 'projectile-globally-ignored-directories "obj")
(add-to-list 'projectile-globally-ignored-directories ".git")
;; --------
;;; - command: helm-projectile-grep =C-c p s g=
;;; - configuration:
;; You can specify directory to exclude when searching by customize either one of these variables:
;;   =grep-find-ignored-files=:  --> List of file names which rgrep and lgrep shall exclude. helm-projectile-grep also uses this variable.
;;   =grep-find-ignored-directories=:  --> List of names of sub-directories which rgrep shall not recurse into. helm-projectile-grep also uses this variable.
;;   =projectile-globally-ignored-files=:  --> A list of files globally ignored by Projectile.
;;   =projectile-globally-ignored-directories=:  --> A list of directories globally ignored by Projectile.
;; --------
;;; - command: helm-projectile-ack =C-c p s a=
;;; - configuration:
;;   =grep-find-ignored-files=: -->  List of file names which rgrep and lgrep shall exclude, and helm-projectile-ack also uses this variable.
;;   =grep-find-ignored-directories=: -->  List of names of sub-directories which rgrep shall not recurse into. helm-projectile-ack also uses this variable.
;;   =projectile-globally-ignored-files=: -->  A list of files globally ignored by Projectile.
;;   =projectile-globally-ignored-directories=: -->  A list of directories globally ignored by Projectile.
;; --------
;;; - command: helm-projectile-ag =C-c p s s=
;;; - configuration:
;;   =grep-find-ignored-files=: --> List of file names which rgrep and lgrep shall exclude, and helm-projectile-ack also uses this variable.
;;   =grep-find-ignored-directories=: --> List of names of sub-directories which rgrep shall not recurse into. helm-projectile-ack also uses this variable.
;;   =projectile-globally-ignored-files=: --> A list of files globally ignored by Projectile.
;;   =projectile-globally-ignored-directories=: --> A list of directories globally ignored by Projectile.


;; Summary of Keybindings
;; C-c p h 	    helm-projectile 	Helm interface to projectile
;; C-c p p 	    helm-projectile-switch-project 	Switches to another projectile project
;; C-c p f 	    helm-projectile-find-file 	Lists all files in a project
;; C-c p F 	    helm-projectile-find-file-in-known-projects 	Find file in all known projects
;; C-c p g 	    helm-projectile-find-file-dwim 	Find file based on context at point
;; C-c p d 	    helm-projectile-find-dir 	Lists available directories in current project
;; C-c p e 	    helm-projectile-recentf 	Lists recently opened files in current project
;; C-c p a 	    helm-projectile-find-other-file 	Switch between files with same name but different extensions
;; C-c p i 	    projectile-invalidate-cache 	Invalidate cache
;; C-c p z 	    projectile-cache-current-file 	Add the file of current selected buffer to cache
;; C-c p b 	    helm-projectile-switch-to-buffer 	List all open buffers in current project
;; C-c p s g 	helm-projectile-grep 	Searches for symbol starting from project root
;; C-c p s a 	helm-projectile-ack 	Same as above but using ack
;; C-c p s s 	helm-projectile-ag 	Same as above but using ag


;; ------------------------------------------------------
;; List of Safe Variable Declaration (suppress warnings)
;; ------------------------------------------------------
(add-to-list 'safe-local-variable-values
             '(project-local-include-path . ("-I./include" "-I./src")))