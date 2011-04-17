;;; sz-testutils.el -- test utilities

;; Copyright (C) 2009 Stephane Zermatten

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

(defmacro sz-testutils-with-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.

CONTENT can be a string or a list of strings with one
symbol 'cursor in it. The point will be put where 'cursor
is: '(\"Here >>\" cursor \"<<.\")
 
The return value is the one returned by BODY."
  `(with-temp-buffer
     (sz-testutils-insert ,content)
     (progn ,@body)))

(defmacro sz-testutils-with-buffer-content (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.

CONTENT can be a string or a list of strings with one
symbol 'cursor in it. The point will be put where 'cursor
is: '(\"Here >>\" cursor \"<<.\")

The return value is the content of the buffer after BODY
has run."
  `(with-temp-buffer
     (sz-testutils-insert ,content)
     (save-excursion
       ,@body)
     (buffer-string)))

(defmacro sz-testutils-with-buffer-region (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.

CONTENT can be a string or a list of strings with one
symbol 'cursor in it. The point will be put where 'cursor
is: '(\"Here >>\" cursor \"<<.\")

The return value is the content of the regionBODY
has run. The mark must be set."
  `(with-temp-buffer
     (sz-testutils-insert ,content)
     (save-excursion
       ,@body
       (buffer-substring-no-properties (point) (mark)))))

(defmacro sz-testutils-with-buffer-ret-and-content (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.

CONTENT can be a string or a list of strings with one
symbol 'cursor in it. The point will be put where 'cursor
is: '(\"Here >>\" cursor \"<<.\")

The return value is a cons containing the return value of 
BODY and the content of the buffer after BODY has run."
  `(with-temp-buffer
     (sz-testutils-insert ,content)
     (cons 
      (save-excursion
	,@body)
      (buffer-string))))

(defun sz-testutils-insert (content)
"Insert CONTENT to the current buffer.

CONTENT can be a string or a list of strings with one
symbol 'cursor in it. The point will be put where 'cursor
is: '(\"Here >>\" cursor \"<<.\")"
  (cond 
   ((stringp content)
    (insert content)
    (goto-char (point-min)))

   ((listp content)
    (let ( (cursor (point-min)) )
      (dolist (element content)
	(cond 
	 ((eq 'cursor element)
	  (setq cursor (point)))
	 ((eq 'mark element)
	  (set-mark (point))
	  (setq mark-active t))
	 ((and (listp element) (eq 'is (car element)))
	  (set (car (cdr element)) (point)))
	 (t
	  (insert element))))
      (goto-char cursor)))

   (t
    (error (format "Neither a string nor a list of strings: %s" content)))))

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'sz-testutils-regress 'regression-suite t)
  (setq sz-testutils-regress
   '("sz-testutils-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("sz-testutils-with-buffer execute body with buffer"
      (sz-testutils-with-buffer "hello" (buffer-string))
      "hello")

     ("sz-testutils-with-buffer return body "
      (sz-testutils-with-buffer "hello" "bobo")
      "bobo")

     ("sz-testutils-with-buffer-content execute body with buffer"
      (sz-testutils-with-buffer-content "hello" (insert (buffer-string)))
      "hellohello")

     ("sz-testutils-with-buffer set cursor position"
      (sz-testutils-with-buffer '("hel" cursor "lo") (point))
      4)

     ("sz-testutils-with-buffer-content set cursor position"
      (sz-testutils-with-buffer-content '("hel" cursor "lo") (insert "X"))
      "helXlo")

     ("sz-testutils-with-buffer store position"
      (let ((p1) (p2))
	(sz-testutils-with-buffer '("hel" (is p1) "lo" (is p2))
				  (list p1 p2)))
      '(4 6))

     ("sz-testutils-with-buffer set mark"
      (sz-testutils-with-buffer '("hel" mark "lo" cursor)
				(list mark-active (mark) (point)))
      '(t 4 6))
     
     ("sz-testutils-with-buffer-region set mark"
      (sz-testutils-with-buffer-region "hello world"
				       (goto-char 4) (push-mark 9))
      "lo wo")

     ("sz-testutils-with-buffer-ret-and-content"
      (sz-testutils-with-buffer-ret-and-content '("hel" cursor "lo") (insert "X") 2)
      '(2 . "helXlo"))

      )))


;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (if (featurep 'regress)
      (regress sz-testutils-regress)))

(provide 'sz-testutils)