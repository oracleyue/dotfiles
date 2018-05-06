;;; emacs-for-python.el --- Python major mode

;; This is only a wraper for use-package of the collection of packages from
;;    https://github.com/gabrielelanaro/emacs-for-python.

(require 'epy-setup)           ;; required!
(setq epy-enable-ropemacs nil) ;; disabling *ropemacs* (set before "epy-python")
(require 'epy-python)          ;; python facilities [optional]
(require 'epy-editing)         ;; editing [optional]
(require 'epy-bindings)        ;; suggested keybindings [optional]
;(require 'epy-nose)            ;; nose integration



(provide 'emacs-for-python)
;;; emacs-for-python.el ends here