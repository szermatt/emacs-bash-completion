;;; pre24.el --- run bash-completion tests under Emacs 22 and 23

;; Copyright (C) 2017 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.


;;; Commentary:
;;
;; This file defines just enough macros and functions to run the
;; bash-completion tests on Emacs 22 and 23 from the command line, using
;; run_tests.sh.
;;
;; When this is used, the tests are run immediately, as they are
;; defined. This file isn't meant to be used from an interactive emacs
;; session.
;;

;;; Code:
(require 'cl)
(require 'dired)

(if (>= emacs-major-version 24)
    (error "Don't use pre24.el with Emacs 24 or later."))

(defvar pre24-error-count 0
  "Number of errors encountered by `ert-deftest'.

`ert-run-tests-batch-and-exit' relies on this to exit with the
appropriate return code.")

(defmacro ert-deftest (name args &rest body)
  "Run a test and reports the result."
  `(let ((pre24-test (symbol-name (quote ,name))))
     (princ (format "%s..." pre24-test))
     (princ (catch 'pre24-return (prog1 "OK" (progn ,@body))))
     (princ "\n")))

(defmacro should (&rest body)
  "Fail the containing `ert-deftest' unless BODY is t."
  `(unless (progn ,@body)
     (setq pre24-error-count (1+ pre24-error-count))
     (throw 'pre24-return
            (format "FAILED\n  (should %s)\n\n"
                    (quote ,@body)))))

(defmacro should-not (&rest body)
  "Fail the containing `ert-deftest' unless BODY is not t."
  `(when (progn ,@body)
    (error "Test failed")))

(defun skip-unless (condition)
  "Skip the test if CONDITION doesn't hold."
  (unless condition (throw 'pre24-return "SKIP")))

(defmacro cl-letf (definitions &rest body)
  "Backward-compatibility adapter for cl-letf.

This macro only supports redefining symbol functions."
  `(flet ,(mapcar
           (lambda (def)
             ;; ((symbol-function ',name) (lambda ,args ,func-body))
             (let ((lhs (car def)) (rhs (cadr def)))
               (unless (and (eq 'symbol-function (car lhs))
                            (eq 'quote (car (nth 1 lhs)))
                            (eq 'lambda (car rhs)))
                 "Unsupported pattern")
               (cons (cadr (cadr (car def))) ; name
                     (cons
                      (car (cdr (cadr def))) ; args
                      (cdr (cdr (cadr def))) ; func-body
                      ))))
           definitions)
     ,@body))


(defun ert-run-tests-batch-and-exit ()
  "Exit tests.

Exit status is 1 if at least on test run with `ert-deftest'
failed."
  (if (> pre24-error-count 0)
      (progn
        (princ "FAIL")
        (kill-emacs 1)))
  (progn
    (princ "PASS\n")
    (kill-emacs 0)))

(provide 'pre24)
;;; pre24.el ends here
