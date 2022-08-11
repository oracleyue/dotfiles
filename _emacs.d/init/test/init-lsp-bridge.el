;; ================================================================
;; Fastest Emacs client for the Language Server Protocol
;; ================================================================
;; Last modified on 12 Jun 2022

;; System Dependencies:
;; - install Python dependencies: python-epc ("pip install epc")
;; - install Elisp dependencies: posframe, markdown-mode, yasnippet
;; - install LSP language servers
;;   - Python: pyright ("pip install pyright")

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge/")
(require 'lsp-bridge)
(defcustom lsp-bridge-lang-server-list
  '(
    ((c-mode c++-mode) . "clangd")
    (python-mode . "pyright")
    (sh-mode . "bash-language-server")
    )
  "The lang server rule for file mode."
  :type 'cons)
(global-lsp-bridge-mode)


(provide 'init-lsp-bridge)
;; ================================================
;; init-lsp-bridge.el ends here
