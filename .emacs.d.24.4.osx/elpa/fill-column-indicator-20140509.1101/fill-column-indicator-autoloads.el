;;; fill-column-indicator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-on-fci-mode fci-mode) "fill-column-indicator"
;;;;;;  "fill-column-indicator.el" (21420 11108 589663 654000))
;;; Generated autoloads from fill-column-indicator.el

(autoload 'fci-mode "fill-column-indicator" "\
Toggle fci-mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
rule: `fci-rule-column', `fci-rule-width', `fci-rule-color',
`fci-rule-use-dashes', `fci-dash-pattern', `fci-rule-character',
and `fci-rule-character-color'.  For further options, see the
Customization menu or the package file.  (See the latter for tips
on troubleshooting.)

\(fn &optional ARG)" t nil)

(autoload 'turn-on-fci-mode "fill-column-indicator" "\
Turn on fci-mode unconditionally.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("fci-osx-23-fix.el" "fill-column-indicator-pkg.el")
;;;;;;  (21420 11109 31503 452000))

;;;***

(provide 'fill-column-indicator-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fill-column-indicator-autoloads.el ends here
