;;; math-at-point.el --- Perform mathematical operations at point or over selection -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/math-at-point
;; Version: 1.0
;; Package-Requires: ((emacs "26") (dash "2.17.0"))
;; Keywords: convenience

;; math-at-point is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; math-at-point is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with math-at-point.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package allows for mathematical operations to be run at point or
;; over a selection.
;; It supports repeating last calculation using `math-at-point-calculation-repeat'

;;; Code:
(require 'calc)
(require 'dash)
(require 'thingatpt)

(defvar math-at-point-last-calculation nil
  "The last calculation. Can be repeated using `math-at-point-calculation-repeat'.")

;; * Calculate
(defun math-at-point-calculate (&optional func thing bounds)
  "The main function used to apply calculations to buffer contents.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
THING is the thing (as defined by the package `thingatpt' in
`bounds-of-thing-at-point'), by default `'number' is used.
BOUNDS allows you to supply the bounds of where the number is located.
This is used to increase performance while running this function over all
numbers in selection."
  (unless (or func math-at-point-last-calculation)
    (error "Error: Can't repeat last calculation as no calculations have been performed yet"))
  (when func
    (setq math-at-point-last-calculation func))
  (let ((pt (point)))
    (save-excursion
      (let* ((num-bounds (or bounds (bounds-of-thing-at-point (or thing 'number))))
	     (num (string-to-number (buffer-substring-no-properties (car num-bounds) (cdr num-bounds))))
	     (result (funcall (or func math-at-point-last-calculation) num)))
	(unless (eq num result)
	  (delete-region (car num-bounds) (cdr num-bounds))
	  (goto-char (car num-bounds))
	  (insert (number-to-string result)))))
    (goto-char pt)))

;; ** Range
(defun math-at-point-calculate-range (func beg end thing thing-regex)
  "A wrapper to apply `math-at-point-calculate' at every line in range.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
BEG and END specifies in what region this function will run.
THING is the thing to apply the calculate operation on (as defined by the
package `thingatpt' in `bounds-of-thing-at-point'), by default `'number'
is used. THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (mapc (apply-partially 'math-at-point-calculate func thing) (math-at-point-get-all-things beg end thing-regex)))

(defun math-at-point-get-all-things (beg end &optional thing-regex)
  "Gets the bounds of all things specified by THING-REGEX in region BEG END.
BEG and END specifies in what region this function will run.
THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (save-excursion
    (let ((numbers '()))
      (goto-char beg)
      (while (and (< (point) end) (search-forward-regexp (or thing-regex "-?[0-9]+\\.?[0-9]*") nil t))
	(push (bounds-of-thing-at-point 'number) numbers)
	(goto-char (cdr (car numbers))))
      numbers)))

;; ** Wrappers
(defun math-at-point-run (func &optional beg end thing thing-regex)
  "Run FUNC at THING at point or if selection is active, run it on every THING
between within selection.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
BEG and END specifies in what region this function will run.
THING is the thing to apply the calculate operation on (as defined by the
package `thingatpt' in `bounds-of-thing-at-point'), by default `'number'
is used. THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (if (and beg end)
      (math-at-point-calculate-range func beg end thing thing-regex)
    (if mark-active
	(math-at-point-calculate-range func (point) (mark) thing thing-regex)
      (math-at-point-calculate func thing nil))))

(defun math-at-point-run-input (prompt func &optional beg end thing thing-regex)
  "Ask the user for a number then apply that number to FUNC.
PROMPT is the prompt text to use for `completing-read'.
FUNC is the calculation to be performed and should only accept two arguments,
the first one is the user input, the second one is the number at point in
the buffer.
BEG and END specifies in what region this function will run.
THING is the thing to apply the calculate operation on (as defined by the
package `thingatpt' in `bounds-of-thing-at-point'), by default `'number'
is used. THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (let* ((input-raw (completing-read prompt nil))
	 (input (string-to-number input-raw)))
    (if (string= input-raw "")
	(math-at-point-repeat-char)
      (math-at-point-run (apply-partially func input) beg end thing thing-regex))))

;; * Thing at point
;; thing-at-point doesn't have `'number' included in `bounds-of-thing-at-point' for whatever reason
;; Some day I will make a pull request for this
(put 'number 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)))
	 (when thing
	   (let ((beginning (match-beginning 0))
		 (end (match-end 0)))
	     (cons beginning end))))))

;; * Operators
;; ** Misc
;;;###autoload
(defun math-at-point-calculation-repeat ()
  "Repeats the last calculation."
  (interactive)
  (math-at-point-calculate))

;;;###autoload
(defun math-at-point-quick-calc ()
  "Run number at point through `quick-calc'."
  (interactive)
  (math-at-point-run
   (lambda (a) (car (calc-do-alg-entry (number-to-string a))))))

;; ** Algebraic
;;;###autoload
(defun math-at-point-add (&optional beg end)
  "Run addition on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run-input "+ " '+ beg end))

;;;###autoload
(defun math-at-point-neg (&optional beg end flip)
  "Run negation on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (math-at-point-run-input "- " '- beg end)
    (math-at-point-run-input "(flip) - " '- beg end)))

;;;###autoload
(defun math-at-point-mult (&optional beg end)
  "Run multiplication on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run-input "* " '* beg end))

;;;###autoload
(defun math-at-point-div (&optional beg end flip)
  "Run division on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (math-at-point-run-input "/ " (-flip '/) beg end)
    (math-at-point-run-input "(flip) / " '/ beg end)))

;;;###autoload
(defun math-at-point-mod (&optional beg end flip)
  "Run modulus on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (math-at-point-run-input "% " (-flip 'mod) beg end)
    (math-at-point-run-input "(flip) % " 'mod beg end)))

;;;###autoload
(defun math-at-point-exp (&optional beg end flip)
  "Set exponent on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (math-at-point-run-input "^ " (-flip 'expt) beg end)
    (math-at-point-run-input "(flip) ^ " 'expt beg end)))

(defalias 'math-at-point-raise 'math-at-point-exp)

;; ** 1 arg
;;;###autoload
(defun math-at-point-add-1 (&optional beg end)
  "Increase number at point by 1.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run (apply-partially '+ 1) beg end))

;;;###autoload
(defun math-at-point-neg-1 (&optional beg end)
  "Decrease number at point by 1.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run (apply-partially (-flip '-) 1) beg end))

;;;###autoload
(defun math-at-point-sqrt (&optional beg end)
  "Run square root on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'sqrt beg end))

;;;###autoload
(defun math-at-point-abs (&optional beg end)
  "Run absolute on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'abs beg end))

;;;###autoload
(defun math-at-point-round (&optional beg end)
  "Round number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'round beg end))

;;;###autoload
(defun math-at-point-floor (&optional beg end)
  "Floor number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'floor beg end))

;;;###autoload
(defun math-at-point-cos (&optional beg end)
  "Run cos on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'cos beg end))

;;;###autoload
(defun math-at-point-sin (&optional beg end)
  "Run sin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'sin beg end))

;;;###autoload
(defun math-at-point-tan (&optional beg end)
  "Run sin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'tan beg end))

;;;###autoload
(defun math-at-point-acos (&optional beg end)
  "Run acos on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'acos beg end))

;;;###autoload
(defun math-at-point-asin (&optional beg end)
  "Run asin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'asin beg end))

;;;###autoload
(defun math-at-point-atan (&optional beg end)
  "Run atan on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (math-at-point-run 'atan beg end))

(provide 'math-at-point)

;;; math-at-point.el ends here
