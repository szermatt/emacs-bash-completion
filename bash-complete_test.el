;;; bash-complete_test.el --- Tests jbtn-complete.el


;;; Commentary:
;;
;; This file defines `bash-complete-regress' and run the
;; regression tests if and only if regress is already imported.
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  ;; force reload
  (load-library "~/.emacs.d/bash-complete.el")

  (require 'sz-testutils)

  ;; This code will not appear in the compiled (.elc) file
  (put 'bash-complete-regress 'regression-suite t)
  (setq bash-complete-regress
   '("bash-complete-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did

     ("bash-complete-join simple"
      (bash-complete-join '("a" "hello" "world" "b" "c"))
      "'a' 'hello' 'world' 'b' 'c'")

     ("bash-complete-join escape quote"
      (bash-complete-join '("a" "hel'lo" "world" "b" "c"))
      "'a' 'hel\\'lo' 'world' 'b' 'c'")

     ("bash-complete-split simple"
      (sz-testutils-with-buffer
       '("a hello world b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-complete-split simple extra spaces"
      (sz-testutils-with-buffer
       '("  a  hello \n world \t b \r c  ")
       (bash-complete-split 1 (line-end-position 2) 0))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-complete-split escaped space"
      (sz-testutils-with-buffer
       '("a hello\\ world b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-complete-split double quotes"
      (sz-testutils-with-buffer
       '("a \"hello world\" b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-complete-split double quotes escaped"
      (sz-testutils-with-buffer
       '("a \"-\\\"hello world\\\"-\" b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "-\"hello world\"-" "b" "c")))

     ("bash-complete-split single quotes"
      (sz-testutils-with-buffer
       '("a 'hello world' b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-complete-split single quotes escaped"
      (sz-testutils-with-buffer
       '("a '-\\'hello world\\'-' b c")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "-'hello world'-" "b" "c")))

     ("bash-complete-split complex quote mix"
      (sz-testutils-with-buffer
       '("a hel\"lo w\"o'rld b'c d")
       (bash-complete-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world bc" "d")))

     ("bash-complete-split cursor at end of word"
      (sz-testutils-with-buffer
       '("a hello world" cursor " b c")
       (bash-complete-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

     ("bash-complete-split cursor in the middle of a word"
      (sz-testutils-with-buffer
       '("a hello wo" cursor "rld b c")
       (bash-complete-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

     ("bash-complete-split cursor at the beginnig"
      (sz-testutils-with-buffer
       '(" " cursor " a hello world b c")
       (bash-complete-split 1 (line-end-position) (point)))
      '(0 . ("a" "hello" "world" "b" "c")))

     ("bash-complete-split cursor in the middle"
      (sz-testutils-with-buffer
       '("a hello " cursor " world b c")
       (bash-complete-split 1 (line-end-position) (point)))
      '(1 . ("a" "hello" "world" "b" "c")))

      )))


;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (if (featurep 'regress)
      (regress bash-complete-regress)))

;;; bash-complete_test.el ends here
