;; ================================================================
;; Debug Adapter Protocol (DAP) Supports
;; ================================================================
;; Last modified on 22 Dec 2024

(use-package dape
  :bind (("<f5>" . dape)
         ("M-<f5>" . dape-hydra/body))

  :pretty-hydra
  ((:title (pretty-hydra-title "Debug" 'codicon "nf-cod-debug")
           :color pink :quit-key ("q" "C-g"))
   ("Stepping"
    (("n" dape-next "next")
     ("s" dape-step-in "step in")
     ("o" dape-step-out "step out")
     ("c" dape-continue "continue")
     ("p" dape-pause "pause")
     ("k" dape-kill "kill"))
    "Switch"
    (("m" dape-read-memory "memory")
     ("t" dape-select-thread "thread")
     ("w" dape-watch-dwim "watch")
     ("S" dape-select-stack "stack")
     ("i" dape-info "info")
     ("R" dape-repl "repl"))
    "Breakpoints"
    (("b" dape-breakpoint-toggle "toggle")
     ("l" dape-breakpoint-log "log")
     ("e" dape-breakpoint-expression "expression")
     ("B" dape-breakpoint-remove-all "clear"))
    "Debug"
    (("d" dape "dape")
     ("r" dape-restart "restart")
     ("D" dape-disconnect-quit "disconnect")
     ("Q" dape-quit "quit" :exit t))))

  :config
  ;; display hydra on startup
  (add-hook 'dape-on-start-hooks #'dape-hydra/body)
  )


(provide 'init-dap)
;; ================================================
;; init-dap.el ends here
