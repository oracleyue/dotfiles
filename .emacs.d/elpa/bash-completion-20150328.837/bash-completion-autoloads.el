;;; bash-completion-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "bash-completion" "bash-completion.el" (21805
;;;;;;  34838 0 0))
;;; Generated autoloads from bash-completion.el

(autoload 'bash-completion-setup "bash-completion" "\
Register bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions'.

This function is convenient, but it might not be the best way of enabling
bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

  (autoload 'bash-completion-dynamic-complete \"bash-completion\"
    \"BASH completion hook\")
  (add-hook 'shell-dynamic-complete-functions
  	  'bash-completion-dynamic-complete)

\(fn)" nil nil)

(autoload 'bash-completion-dynamic-complete "bash-completion" "\
Returns the completion table for bash command at point.

This function is meant to be added into
`shell-dynamic-complete-functions'.  It uses `comint' to figure
out what the current command is and returns a completion table or
nil if no completions available.

\(fn)" nil nil)

(autoload 'bash-completion-reset "bash-completion" "\
Force the next completion command to start with a fresh BASH process.

This function kills any existing BASH completion process.  This way, the
next time BASH completion is requested, a new process will be created with
the latest configuration.

Call this method if you have updated your .bashrc or any bash init scripts
and would like bash completion in Emacs to take these changes into account.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("bash-completion-pkg.el" "sz-testutils.el")
;;;;;;  (21805 34838 186437 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bash-completion-autoloads.el ends here
