; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; For /Icomplete Mode/
(icomplete-mode 1)    ; Turn on icomplete-mode

;; /ido, ido-ubiquitous, flx-ido/
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

;; Baisc configuration
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/userdata/ido.hist"
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;; Enable modes
(ido-mode t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)          ;smarter fuzzy matching for ido

;; Keybindings
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; Extensions ordering and ignored ones
(setq ido-file-extensions-order '(".tex" ".org" ".py" ".cpp" ".hpp" ".sh" ".xml" ".el"))
(setq completion-ignored-extensions '(".o" ".elc" "~" ".obj" ".a" ".so" ".aux" ".out" ".pyg" "blg" "log" ".synctex.gz" "toc" "bbl"))
(setq ido-ignore-extensions t)

;; Keep annoying buffers out of search
(setq ido-ignore-buffers (list (rx (or (and bos  " ")
                                       (and bos
                                            (or "*Completions*"
                                                "*Shell Command Output*"
                                                "*vc-diff*")
                                            eos)))))
;; Show ido results vertically, rather than horizontally
;(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
;  [No match]" " [Matched]" " [Not readable]" " [Too big]" "
;  [Confirm]")))
;(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
 
;; /smex/ remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file "~/.emacs.d/userdata/.smex-items")
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;/ido-hacks/
(require 'ido-hacks nil t)