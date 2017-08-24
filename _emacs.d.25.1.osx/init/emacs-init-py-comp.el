; =======================================
;; Programming Environment for /Python/

;; USAGES (/emacs-for-python/):
;; ------------------
;; *edit*
;;   - shift selected blocks "C-c >", "C-c <"
;; *debug*:
;;   - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;;   - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts; then evaluate buffer in iPython
;; *rope refactorings*:
;;   - "C-x p o" open rope project
;;   - "C-x p f" find file in rope project
;;   - "C-c d" python doc for module/method
;;   - "C-c g" go to definition
;;   - "C-c f" find occurencies
;;   - "C-c r r" refactoring rename
;; *nose*:



;;
;; Setup /emacs-for-python/ major modes (IDE)
;;

;; Load by default
;(load-file "~/.emacs.d/git/emacs-for-python/epy-init.el")
;; custom load
(add-to-list 'load-path "~/.emacs.d/git/emacs-for-python/")
(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
;(require 'epy-completion) ;; If you want the autocompletion settings [optional] ;; use /jedi/
;(require 'epy-editing)    ;; For configurations related to editing [optional]
;(require 'epy-bindings)   ;; For my suggested keybindings [optional]
;(require 'epy-nose)       ;; For nose integration

; use *IPython*
(epy-setup-ipython)

; use *flymake* checker
(epy-setup-checker "pyflakes %f")

; Edit: *indentation and line highlight*
(defun epy-edit-hl-config()
  ;; * /highlight-indentation/ in "epy-editing.el"
  (require 'highlight-indentation)
  (highlight-indentation)
  (when (eq 'ymonokai (car custom-enabled-themes))
    (set-face-attribute 'highlight-indent-face nil
                        :background "#49483E"))
  ;; * highlight line; face by default
  (hl-line-mode t))
(add-hook 'python-mode-hook 'epy-edit-hl-config)

; disabling parenthesis auto-pairing, use /smartparen/ in .emacs
(setq skeleton-pair nil)

; disabling *ropemacs* (nil)
(setq epy-enable-ropemacs nil)

; adding snippets in /emacs-for-python/ when use the default yasnippets
;; having defined in "emacs-init-ac.el" to avoid yas-reload-all again
(add-to-list 'yas-snippet-dirs "~/.emacs.d/git/emacs-for-python/extensions/yasnippet/snippets")


;;
;; Auto-completion by /Jedi/, using /company-jedi/
;;
;; (add-to-list 'company-backends 'company-jedi)
(defun y:company-py-setup ()
  (setq-local company-backends
              (append '(company-jedi) company-backends)))
(add-hook 'python-mode-hook 'y:company-py-setup)
;; set calltip methods
(setq jedi:tooltip-method nil)  ;popup, pos-tip OR nil (use minibuffer)
