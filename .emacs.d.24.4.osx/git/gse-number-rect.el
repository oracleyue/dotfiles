;; gse-number-rect.el -- Inserts incremental numbers in a rectangle.
;;
;; Copyright (C)    2006 Scott Evans <gse@antisleep.com>
;; Keywords:        extensions
;; Author:          Scott Evans
;; Maintainer:      Scott Evans
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.
;;
;; Time-stamp:  <2006.01.05 14:51:34 gse>
;;
;; Commentary:
;;  Pretty self-expanatory.  If you have text like:
;;     --------------------
;;     Some text here
;;     Another line here
;;     And a final line
;;     --------------------
;;  After selecting a rectangle at the beginning of the lines,
;;  gse-number-rectangle could end you up with
;;     --------------------
;;     1 - Some text here
;;     2 - Another line here
;;     3 - And a final line
;;     --------------------
;; ...assuming you specified a suffix of " - ".
;;
;; I find it especially useful to rename files in conjunction with
;; wdired.
;;
;; Thanks to Juan Leon Lahoz Garcia <juan-leon.lahoz@tecsidel.es>
;; for testing and suggestions.
;;
;; Latest version:
;;   Should be at http://www.antisleep.com/elisp.
;;
;; Installation:
;;   (require 'gse-number-rect)
;;   (global-set-key "\C-xru" 'gse-number-rectangle)
;;
;;---------------------------------------------------------------------------
;; Change Log
;; ----------
;; 2009.06.21 gse Add GPL.
;; 2006.01.05 gse Modify params to support Emacs 22.
;; 2002.04.08 gse Fix off-by-one error in longest computation.
;;                Add gse-number-rectangle-min-width.
;;                Use 'force' parameter for move-to-column.
;; 2002.04.08 gse Use read-string if read-number not bound (FSF).
;; 2002.04.05 gse Add documentation.
;;---------------------------------------------------------------------------

(require 'rect)

;;---------------------------------------------------------------------------

(defvar gse-number-rectangle-min-width nil
  "*If non-nil, an integer that dictates the minimum width
of numbered rectangles.  This controls minimum zero-padding;
i.e. if it is 2, less than 10 lines will still be numbered
01, 02, 03, etc.  If it is 3, 001, 002, 003.")

;;---------------------------------------------------------------------------

(defvar gse-number-rectangle-count nil)
(defvar gse-number-rectangle-history nil)

;;---------------------------------------------------------------------------

(defun gse-number-rectangle-callback (start end format-string suffix-text)
  (move-to-column start t)
  (setq gse-number-rectangle-count (+ gse-number-rectangle-count 1))
  (insert (format format-string gse-number-rectangle-count) suffix-text))

;;---------------------------------------------------------------------------

(defun gse-number-rectangle (start end start-at suffix-text &optional prefix)
  "Insert rising numbers on each line of the region-rectangle, shifting text right.
The left edge of the rectangle specifies the column for insertion.  Numbers are
zero-padded by default.

START-AT specifices the first number to start at.
SUFFIX-TEXT is inserted after the numbers.

Variable gse-number-rectangle-min-width controls minimum zero-padding;
i.e. if it is 2, less than 10 lines will still be numbered 01, 02, 03, etc.
If it is 3, 001, 002, 003.

Optional prefix means do not zero-pad numbers, regardless of
gse-number-rectangle-min-width."
  (interactive
   (list
    (region-beginning) ; start
    (region-end) ; end
    (if (functionp 'read-number)
        (condition-case nil
            (read-number "First number [1]: " t "1")
          (wrong-number-of-arguments
           (read-number "First number: " 1)))
      (string-to-int (read-string "First number [1]: " nil nil "1"))) ; start-at
    (read-string "Suffix: " nil 'gse-number-rectangle-history) ; suffix-text
    current-prefix-arg)) ; prefix

  (setq start-at (- start-at 1))

  (let ((format-string "%i"))
    (if (not prefix)
        (let* ((max (+ (count-lines start end) start-at))
               (longest (length (int-to-string (+ 1 max)))))
          (if (and (integerp gse-number-rectangle-min-width)
                   (< longest gse-number-rectangle-min-width))
              (setq longest gse-number-rectangle-min-width))
          (setq format-string (concat "%0" (int-to-string longest) "i"))))

    (setq gse-number-rectangle-count start-at)
    (apply-on-rectangle 'gse-number-rectangle-callback start end format-string suffix-text)))

;;---------------------------------------------------------------------------

(provide 'gse-number-rect)


