;;; bash-completion_test.el --- Tests bash-completion.el


;;; Commentary:
;;
;; This file defines `bash-completion-regress' and run the
;; regression tests if and only if regress is already imported.
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  ;; force reload
  (load-library "~/.emacs.d/bash-completion.el")

  (require 'sz-testutils)

  ;; This code will not appear in the compiled (.elc) file
  (put 'bash-completion-regress 'regression-suite t)
  (setq bash-completion-regress
   '("bash-completion-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did

     ("bash-completion-join simple"
      (bash-completion-join '("a" "hello" "world" "b" "c"))
      "a hello world b c")

     ("bash-completion-join escape quote"
      (bash-completion-join '("a" "hel'lo" "world" "b" "c"))
      "a 'hel'\\''lo' world b c")

     ("bash-completion-join escape space"
      (bash-completion-join '("a" "hello world" "b" "c"))
      "a 'hello world' b c")

     ("bash-completion-split simple"
      (sz-testutils-with-buffer
       '("a hello world b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split simple extra spaces"
      (sz-testutils-with-buffer
       '("  a  hello \n world \t b \r c  ")
       (bash-completion-split 1 (line-end-position 2) 0))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split escaped space"
      (sz-testutils-with-buffer
       '("a hello\\ world b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split double quotes"
      (sz-testutils-with-buffer
       '("a \"hello world\" b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split double quotes escaped"
      (sz-testutils-with-buffer
       '("a \"-\\\"hello world\\\"-\" b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "-\"hello world\"-" "b" "c")))

     ("bash-completion-split single quotes"
      (sz-testutils-with-buffer
       '("a \"hello world\" b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split single quotes escaped"
      (sz-testutils-with-buffer
       '("a '-\\'hello world\\'-' b c")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "-'hello world'-" "b" "c")))

     ("bash-completion-split complex quote mix"
      (sz-testutils-with-buffer
       '("a hel\"lo w\"o'rld b'c d")
       (bash-completion-split 1 (line-end-position) 0))
      '(nil . ("a" "hello world bc" "d")))

     ("bash-completion-split cursor at end of word"
      (sz-testutils-with-buffer
       '("a hello world" cursor " b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split cursor in the middle of a word"
      (sz-testutils-with-buffer
       '("a hello wo" cursor "rld b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split cursor at the beginnig"
      (sz-testutils-with-buffer
       '(" " cursor " a hello world b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(0 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split cursor in the middle"
      (sz-testutils-with-buffer
       '("a hello " cursor " world b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(1 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-add-to-alist garbage"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("just" "some" "garbage")))
      nil)

     ("bash-completion-add-to-alist empty"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '()))
      nil)

     ("bash-completion-add-to-alist empty string"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("")))
      nil)

     ("bash-completion-add-to-alist empty complete"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("complete")))
      nil)

     ("bash-completion-add-to-alist one command"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("complete" "-e" "-F" "_cdargs_aliases" "cdb")))
      '(("cdb" . ("-e" "-F" "_cdargs_aliases"))))

     ("bash-completion-build-alist"
      (sz-testutils-with-buffer
       "
complete -F _cdargs_aliases cdb
complete -F complete_projects project
complete -F complete_projects pro
complete -F _cdargs_aliases cv
complete -F _cdargs_aliases cb
garbage
"
       (let ((bash-completion-alist '(garbage)))
	 (bash-completion-build-alist (current-buffer))))
      '(("cdb" "-F" "_cdargs_aliases")
	("project" "-F" "complete_projects")
	("pro" "-F" "complete_projects")
	("cv" "-F" "_cdargs_aliases")
	("cb" "-F" "_cdargs_aliases")))

     ("bash-completion-quote not necessary"
      (bash-completion-quote "hello")
      "hello")

     ("bash-completion-quote space"
      (bash-completion-quote "hello world")
      "'hello world'")

     ("bash-completion-quote quote"
      (bash-completion-quote "hell'o")
      "'hell'\\''o'")

     ("bash-completion-generate-line no custom completion"
      (let ((bash-completion-alist nil))
	(bash-completion-generate-line "~/test" "hello worl" 7 '("hello" "worl") 1))
      (concat "cd " (expand-file-name "~/test") " ; compgen -o default worl"))

     ("bash-completion-generate-line custom completion no function or command"
      (let ((bash-completion-alist '(("zorg" . ("-A" "-G" "*.txt")))))
	(bash-completion-generate-line "/test" "zorg worl" 7 '("zorg" "worl") 1))
      "cd /test ; compgen -A -G '*.txt' -- worl")

     ("bash-completion-generate-line custom completion function"
      (let ((bash-completion-alist '(("zorg" . ("-F" "__zorg")))))
	(bash-completion-generate-line "/test" "zorg worl" 7 '("zorg" "worl") 1))
      "cd /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"$@\"' compgen -F __bash_complete_wrapper -- worl")

     ("bash-completion-generate-line custom completion command"
      (let ((bash-completion-alist '(("zorg" . ("-C" "__zorg")))))
	(bash-completion-generate-line "/test" "zorg worl" 7 '("zorg" "worl") 1))
      "cd /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"$@\"' compgen -F __bash_complete_wrapper -- worl")

     ("bash-completion-trim"
      (mapcar 'bash-completion-trim '("  hello " "  world   " "x"))
      '("hello" "world" "x"))

     ("bash-completion-line-beginning-position start"
      (sz-testutils-with-buffer
       "cd /home/x"
       (bash-completion-line-beginning-position 1))
      1)

     ("bash-completion-line-beginning-position semicolon"
      (sz-testutils-with-buffer
       '("cd /home/x ; " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(14 14))

     ("bash-completion-line-beginning-position &&"
      (sz-testutils-with-buffer
       '("cd /home/x && " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(15 15))

      )))


;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (if (featurep 'regress)
      (regress bash-completion-regress)))

;;; bash-completion_test.el ends here
