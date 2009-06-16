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
      "a hello world b c")

     ("bash-complete-join escape quote"
      (bash-complete-join '("a" "hel'lo" "world" "b" "c"))
      "a 'hel'\\''lo' world b c")

     ("bash-complete-join escape space"
      (bash-complete-join '("a" "hello world" "b" "c"))
      "a 'hello world' b c")

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
       '("a \"hello world\" b c")
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

     ("bash-complete-add-to-alist garbage"
      (let ((bash-complete-alist nil))
	(bash-complete-add-to-alist '("just" "some" "garbage")))
      nil)

     ("bash-complete-add-to-alist empty"
      (let ((bash-complete-alist nil))
	(bash-complete-add-to-alist '()))
      nil)

     ("bash-complete-add-to-alist empty string"
      (let ((bash-complete-alist nil))
	(bash-complete-add-to-alist '("")))
      nil)

     ("bash-complete-add-to-alist empty complete"
      (let ((bash-complete-alist nil))
	(bash-complete-add-to-alist '("complete")))
      nil)

     ("bash-complete-add-to-alist one command"
      (let ((bash-complete-alist nil))
	(bash-complete-add-to-alist '("complete" "-e" "-F" "_cdargs_aliases" "cdb")))
      '(("cdb" . ("-e" "-F" "_cdargs_aliases"))))

     ("bash-complete-build-alist"
      (sz-testutils-with-buffer
       "
complete -F _cdargs_aliases cdb
complete -F complete_projects project
complete -F complete_projects pro
complete -F _cdargs_aliases cv
complete -F _cdargs_aliases cb
garbage
"
       (let ((bash-complete-alist '(garbage)))
	 (bash-complete-build-alist (current-buffer))))
      '(("cdb" "-F" "_cdargs_aliases")
	("project" "-F" "complete_projects")
	("pro" "-F" "complete_projects")
	("cv" "-F" "_cdargs_aliases")
	("cb" "-F" "_cdargs_aliases")))

     ("bash-complete-quote not necessary"
      (bash-complete-quote "hello")
      "hello")

     ("bash-complete-quote space"
      (bash-complete-quote "hello world")
      "'hello world'")

     ("bash-complete-quote quote"
      (bash-complete-quote "hell'o")
      "'hell'\\''o'")

     ("bash-complete-generate-line no custom completion"
      (let ((bash-complete-alist nil))
	(bash-complete-generate-line "hello worl" 7 '("hello" "worl") 1))
      "compgen -o default worl")

     ("bash-complete-generate-line custom completion no function or command"
      (let ((bash-complete-alist '(("zorg" . ("-A" "-G" "*.txt")))))
	(bash-complete-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "compgen -A -G '*.txt' worl")

     ("bash-complete-generate-line custom completion function"
      (let ((bash-complete-alist '(("zorg" . ("-F" "__zorg")))))
	(bash-complete-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POS=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"$@\"' compgen -F __bash_complete_wrapper worl")

     ("bash-complete-generate-line custom completion command"
      (let ((bash-complete-alist '(("zorg" . ("-C" "__zorg")))))
	(bash-complete-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POS=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"$@\"' compgen -F __bash_complete_wrapper worl")

     ("bash-complete-trim"
      (mapcar 'bash-complete-trim '("  hello " "  world   " "x"))
      '("hello" "world" "x"))

      )))


;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (if (featurep 'regress)
      (regress bash-complete-regress)))

;;; bash-complete_test.el ends here
