;; Function: batch convert .md files from .org files marked in Dired
;; Usage: M-x this function after marking files in Dired
(defun dired-org-to-md ()
  (interactive)
  (mapc
   (lambda (f)
     (with-current-buffer
         (find-file-noselect f)
       (org-md-export-to-markdown)))
   (dired-get-marked-files)))
