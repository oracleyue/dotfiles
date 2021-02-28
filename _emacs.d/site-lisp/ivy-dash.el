;; This script provides a function that uses Ivy to search items in Dash (app in
;; Mac).  From https://emacs-china.org/t/emacs-dash/8108/3
;;
;; Last modified on 03 Jan 2019

(require 'ivy)

(defun dash-in-ivy ()
  (interactive)
  (require 'dom)
  (ivy-read
   "Search Dash: "
   (lambda (str)
     (or
      (ivy-more-chars)
      (with-temp-buffer
        ;; dashAlfredWorkflow 'c:puts' | xmllint --format -
        (if (zerop (call-process "dashAlfredWorkflow" nil t nil str))
            (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
                   (items (dom-by-tag dom 'item)))
              (cl-loop for item in items
                       for idx from 0
                       for uid = (dom-attr item 'uid)
                       for quicklookurl = (dom-text (dom-child-by-tag item 'quicklookurl))
                       for title = (dom-text (dom-child-by-tag item 'title))
                       for subtitle = (dom-text (car (last (dom-by-tag item 'subtitle))))
                       for subtitle+face = (propertize subtitle 'face 'font-lock-comment-face)
                       collect (propertize (concat title " " subtitle+face)
                                           'uid uid
                                           'idx idx
                                           'quicklookurl quicklookurl)))
          (list
           "Error: dashAlfredWorkflow fails"
           ""
           (split-string (buffer-string) "\n"))))))
   :dynamic-collection t
   :action (lambda (x)
             (call-process "open"
                           nil nil nil
                           "-g" (format "dash-workflow-callback://%d"
                                        (get-text-property 0 'idx x))))))
(ivy-set-actions
 'dash-in-ivy
 '(("b"
    (lambda (x)
      (browse-url (get-text-property 0 'quicklookurl x)))
    "browse url")))


(provide 'ivy-dash)
;; ================
;; ivy-dash.el ends here