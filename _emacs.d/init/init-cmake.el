;; =====================================================
;; Programming Supports for CMake
;; =====================================================
;; Last modified on 02 Mar 2021

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'"
         "\\.cmake\\'")
  :hook ((cmake-mode . zyue/company-cmake-setup)
         (cmake-mode . zyue/cmake-setup-compile))
  :config
  ;; /cmake-font-lock/: better fontifying
  (use-package cmake-font-lock
    :hook (cmake-mode . cmake-font-lock-activate))

  ;; auto completion
  (add-to-list 'company-dabbrev-code-modes 'cmake-mode)
  (defun zyue/company-cmake-setup ()
    (setq-local company-backends
                (append '((company-cmake company-dabbrev-code))
                        company-backends)))

  ;; compile setup
  (defun zyue/link-compile-commands-json (buffer msg)
    "Linking compile_commands.json to project root for LSP clangd."
    (let ((link-status nil))
      (when (string-match "^finished" msg)
        (shell-command "cd ..; ln -s build/compile_commands.json .")
        (setq link-status t))
      (with-current-buffer (get-buffer "*compilation*")
        (goto-char (point-max))
        (if link-status
            (insert "\nLinking compile_commands.json for LSP: Done :-) \n")
          (ding)
          (insert "\nLinking compile_commands.json for LSP: Failed :-( \n")))))

  (defun zyue/cmake-setup-compile ()
    (setq compile-command "cd build/ && cmake .. && make -k")
    (add-hook 'compilation-finish-functions 'zyue/link-compile-commands-json)
    (define-key cmake-mode-map (kbd "C-c C-c") 'compile))

  (defun clean-all ()
    (interactive)
    (when (file-directory-p "build")
      (delete-directory "build" t) (delete-file "compile_commands.json")
      (make-directory "build"))))


(provide 'init-cmake)
;; ================================================
;; init-cmake.el ends here
