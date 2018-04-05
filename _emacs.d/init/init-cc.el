; =====================================================
;; Programming Environment for C/C++
; =====================================================
;; Warning: semantic-mode in CEDET causes "M-x gdb" freeze emacs on OSX!
;; Features:
;;   use "C-c h i" to show symbol reference table
;;   create ".dir-local.el" to enable completion for local codes
;;   use helm-projectile to browse files in project
;;      - "C-c p a" to switch between .h, .c and .cpp
;;      - jump to "f" (file); "d" (directory); "b" (buffer); "e" (recent files)
;;      - grep in project: "C-c p g s"
;;      - multi-occur in project buffers: "C-c p o"
;;   use helm-gtags to jump via tags
;;      - use "C-c g c" create tags first and "C-c g u" to update
;;      - use "M-." and "M-," to jump and jump back (see more in "init-tags.el")


;; Load the corresponding complete engine
(cond ((string-equal y:cc-complete-engine "clang")       ;; company-clang
       (require 'init-cc-comp))
      ((string-equal y:cc-complete-engine "irony")       ;; irony
       (org-babel-load-file
        (expand-file-name "init/init-cc-irony.org"
                          user-emacs-directory)))
      ((string-equal y:cc-complete-engine "modern")      ;; rtags + irony
       (org-babel-load-file
        (expand-file-name "init/init-cc-modern.org"
                          user-emacs-directory))))


(provide 'init-cc)
;; ================================================
;; init-cc.el ends here
