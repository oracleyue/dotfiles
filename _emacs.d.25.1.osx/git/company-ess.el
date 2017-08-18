;;; company-ess.el --- R/ess Completion Backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014

;; Author:  Lompik
;; Keywords: completion, ess
;; Version: 0.0.2
;; URL: https://github.com/Lompik/company-ess
;; Package-Requires: ((company "0.8.0") (ess "13.05") (cl-lib "0.5") (emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; R/ESS Completion Backend for company-mode

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ess)

;;; INTERNALS

(defun company-ess-get-compfromr (symb)
  "Call R internal completion utilities (rcomp) for possible completions.
Argument SYMB: Symbol to feed R completion."
  (let* ((comm (format ".ess_get_completions(\"%s\", %d)\n"
		       (ess-quote-special-chars symb)
		       (length symb))))
    (ess-get-words-from-vector comm)))

(defun company-ess-args (symb)
  "Get the args of the function when inside parentheses.
Argument SYMB: Symbol to feed R completion."
  (when  ess--funname.start ;; stored by a coll to ess-ac-start-args
    (let ((args (nth 2 (ess-function-arguments (car ess--funname.start))))
          (len (length symb)))
      (delete "..." args)
      (mapcar (lambda (a) (concat a ess-ac-R-argument-suffix))
              args))))


(defun ess-company-candidates ( symb)
  (let ((args (company-ess-args symb))
	(comps (cdr (company-ess-get-compfromr symb))))
    
    (if args
	(setq comps (append
		     (delq nil (mapcar (lambda (x)
					 (if (string-match symb x)
					     x)) args))
		     comps)))
    comps))

(defun company-ess-start-args () ;Same as ess-ac-start-args
  "Get initial position for args completion."
  (when (and ess-local-process-name
             (not (eq (get-text-property (point) 'face) 'font-lock-string-face)))
    (when (ess--funname.start)
      (if (looking-back "[(,]+[ \t\n]*")
          (point)
        (ess-symbol-start)))))


(defun company-ess-start ()
  (when (and ess-local-process-name
             (get-process ess-local-process-name))
    (let ((start (or (company-ess-start-args)  (ess-symbol-start))))
      (when start
	(buffer-substring-no-properties start (point))))))


(defun company-ess-get-typeof (symb)
  "Call R internal completion utilities (typeof) for possible completions.
Argument SYMB: Symbol to feed R completion."
  (let* ((comm (format "typeof(%s)\n"
		       symb)))
    (format " %.3s" (car (ess-get-words-from-vector comm)))))


(defun company-ess-create-doc-buffer (syms)
  (let ((doc (ess-ac-help syms)))
    (company-doc-buffer doc)))

;;; BACKENDS

;;;###autoload
(defun company-ess-backend (command &optional arg &rest ignored)
  "R/ESS backend for company-mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ess-backend))
    (prefix (company-ess-start))
    (candidates (ess-company-candidates arg))
    (doc-buffer (company-ess-create-doc-buffer arg))
    ;(meta (funcall ess-eldoc-function) )
    ;(annotation (company-ess-get-typeof arg))
    (sorted t) ; get arguments on top of the list
    (duplicates nil)
    ))


(provide 'company-ess)
;;; company-ESS.el ends here
