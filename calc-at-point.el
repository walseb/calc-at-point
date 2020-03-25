;;; calc-at-point.el --- Perform calculations at point or over selection -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/calc-at-point
;; Version: 1.0
;; Package-Requires: ((emacs "26") (dash "2.12.0"))
;; Keywords: convenience

;; calc-at-point is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; calc-at-point is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calc-at-point.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package allows for calculations to be run at point or
;; over a selection.
;; It supports repeating last calculation using `calc-at-point-repeat-last'

;;; Code:
(require 'calc)
(require 'dash)
(require 'thingatpt)

(defvar calc-at-point-last-calculation nil
  "The last calculation. Can be repeated using `calc-at-point-repeat-last'.")

;; * Calculate
(defun calc-at-point-calculate (func &optional thing bounds)
  "The main function used to apply calculations to buffer contents.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
THING is the thing (as defined by the package `thingatpt' in
`bounds-of-thing-at-point'), by default `'number' is used.
BOUNDS allows you to supply the bounds of where the number is located.
This is used to increase performance while running this function over all
numbers in selection."
  (let ((pt (point)))
    (save-excursion
      (let* ((num-bounds (or bounds (bounds-of-thing-at-point (or thing 'number))))
	     (num (string-to-number (buffer-substring-no-properties (car num-bounds) (cdr num-bounds))))
	     (result (funcall func num)))
	(unless (eq num result)
	  (delete-region (car num-bounds) (cdr num-bounds))
	  (goto-char (car num-bounds))
	  (insert (number-to-string result)))))
    (goto-char pt)))

;; ** Range
(defun calc-at-point-calculate-range (func beg end &optional thing thing-regex)
  "A wrapper to apply `calc-at-point-calculate' at every line in range.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
BEG and END specifies in what region this function will run.
THING is the thing to apply the calculate operation on (as defined by the
package `thingatpt' in `bounds-of-thing-at-point'), by default `'number'
is used. THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (mapc (apply-partially 'calc-at-point-calculate func thing) (calc-at-point-get-all-things beg end thing-regex)))

(defun calc-at-point-get-all-things (beg end &optional thing-regex)
  "Gets the bounds of all things specified by THING-REGEX in region BEG END.
BEG and END specifies in what region this function will run.
THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (save-excursion
    (let* ((numbers '())
	   (largest (max beg end))
	   (smallest (min beg end)))
      (goto-char smallest)
      (while (and (< (point) largest) (search-forward-regexp (or thing-regex "-?[0-9]+\\.?[0-9]*") nil t))
	(push (bounds-of-thing-at-point 'number) numbers)
	(goto-char (cdr (car numbers))))
      numbers)))

;; ** Wrappers
(defun calc-at-point-run (&optional func beg end thing thing-regex)
  "Run FUNC at THING at point or if selection is active, run it on every THING
between within selection.
FUNC is the calculation to be performed and should only accept one argument,
that argument will be the number at point.
BEG and END specifies in what region this function will run.
THING is the thing to apply the calculate operation on (as defined by the
package `thingatpt' in `bounds-of-thing-at-point'), by default `'number'
is used. THING-REGEX is a regex of what the THING looks like,
used to quickly collect the bounds of all THINGs in the buffer."
  (when func
    (setq calc-at-point-last-calculation func))

  (let ((func-final (or func calc-at-point-last-calculation)))
    (unless func-final
      (error "Error: Can't repeat last calculation as no calculations have been performed yet"))

    (if (and beg end)
	(calc-at-point-calculate-range func-final beg end thing thing-regex)
      (if mark-active
	  (calc-at-point-calculate-range func-final (point) (mark) thing thing-regex)
	(calc-at-point-calculate func-final thing nil)))))

(defun calc-at-point-run-input (prompt func &optional beg end thing thing-regex)
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
	(calc-at-point-repeat-char)
      (calc-at-point-run (apply-partially func input) beg end thing thing-regex))))

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
(defun calc-at-point-repeat-last ()
  "Repeats the last calculation."
  (interactive)
  (calc-at-point-calculate))

;;;###autoload
(defun calc-at-point-quick-calc ()
  "Run number at point through `quick-calc'."
  (interactive)
  (calc-at-point-run
   (lambda (a) (car (calc-do-alg-entry (number-to-string a))))))

;; ** 2 arg
;;;###autoload
(defun calc-at-point-add (&optional beg end)
  "Run addition on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run-input "+ " '+ beg end))

;;;###autoload
(defun calc-at-point-neg (&optional beg end flip)
  "Run negation on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (calc-at-point-run-input "- " '- beg end)
    (calc-at-point-run-input "(flip) - " '- beg end)))

;;;###autoload
(defun calc-at-point-mult (&optional beg end)
  "Run multiplication on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run-input "* " '* beg end))

;;;###autoload
(defun calc-at-point-div (&optional beg end flip)
  "Run division on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (calc-at-point-run-input "/ " (-flip '/) beg end)
    (calc-at-point-run-input "(flip) / " '/ beg end)))

;;;###autoload
(defun calc-at-point-mod (&optional beg end flip)
  "Run modulus on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (calc-at-point-run-input "% " (-flip 'mod) beg end)
    (calc-at-point-run-input "(flip) % " 'mod beg end)))

;;;###autoload
(defun calc-at-point-exp (&optional beg end flip)
  "Set exponent on number at point.
BEG and END specifies in what region this function will run.
FLIP flips the order the operation is made in."
  (interactive)
  (if flip
      (calc-at-point-run-input "^ " (-flip 'expt) beg end)
    (calc-at-point-run-input "(flip) ^ " 'expt beg end)))

(defalias 'calc-at-point-raise 'calc-at-point-exp)

;; ** 1 arg
;;;###autoload
(defun calc-at-point-add-1 (&optional beg end)
  "Increase number at point by 1.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run (apply-partially '+ 1) beg end))

;;;###autoload
(defun calc-at-point-neg-1 (&optional beg end)
  "Decrease number at point by 1.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run (apply-partially (-flip '-) 1) beg end))

;;;###autoload
(defun calc-at-point-sqrt (&optional beg end)
  "Run square root on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'sqrt beg end))

;;;###autoload
(defun calc-at-point-abs (&optional beg end)
  "Run absolute on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'abs beg end))

;;;###autoload
(defun calc-at-point-round (&optional beg end)
  "Round number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'round beg end))

;;;###autoload
(defun calc-at-point-floor (&optional beg end)
  "Floor number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'floor beg end))

;;;###autoload
(defun calc-at-point-cos (&optional beg end)
  "Run cos on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'cos beg end))

;;;###autoload
(defun calc-at-point-sin (&optional beg end)
  "Run sin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'sin beg end))

;;;###autoload
(defun calc-at-point-tan (&optional beg end)
  "Run sin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'tan beg end))

;;;###autoload
(defun calc-at-point-acos (&optional beg end)
  "Run acos on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'acos beg end))

;;;###autoload
(defun calc-at-point-asin (&optional beg end)
  "Run asin on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'asin beg end))

;;;###autoload
(defun calc-at-point-atan (&optional beg end)
  "Run atan on number at point.
BEG and END specifies in what region this function will run."
  (interactive)
  (calc-at-point-run 'atan beg end))

(provide 'calc-at-point)

;;; calc-at-point.el ends here
