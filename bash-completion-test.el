;;; bash-completion_test.el --- Tests bash-completion.el

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


;;; Commentary:
;;
;; This file defines unit and integrations ERT tests for
;; `bash-completion'.
;;

;;; History:
;;

;;; Code:
(require 'ert)
(require 'sz-testutils)
(require 'cl)
(require 'bash-completion)

(defun bash-completion-test-send (buffer-content)
  "Run `bash-completion-send' on BUFFER-CONTENT.
Return (const return-value new-buffer-content)"
  (let ((process 'process))
    (cl-letf (((symbol-function 'process-buffer)
	       (lambda (process)
		 (unless (eq process 'process)
		   (error "unexpected: %s" process))
		 (current-buffer)))
	      ((symbol-function 'process-send-string)
	       (lambda (process command)
		 (unless (eq process 'process)
		   (error "unexpected process: %s" process))
		 (unless (equal "cmd\n" command)
		   (error "unexpected command: %s" command))))
	      ((symbol-function 'accept-process-output)
	       (lambda (process timeout)
		 (unless (eq process 'process)
		   (error "unexpected process: %s" process))
		 (unless (= timeout 3.14)
		   (error "unexpected timeout: %s" timeout))
		 (insert buffer-content)
		 t)))
      (sz-testutils-with-buffer-ret-and-content
       ""
       (bash-completion-send "cmd" 'process 3.14)))))

;; ---------- unit tests
(ert-deftest bash-completion-join-test ()
  (should (equal "a hello world b c"
		 (bash-completion-join '("a" "hello" "world" "b" "c"))))
  (should (equal "a 'hel'\\''lo' world b c"
		 (bash-completion-join '("a" "hel'lo" "world" "b" "c"))))
  (should (equal "a 'hello world' b c"
		 (bash-completion-join '("a" "hello world" "b" "c")))))

(ert-deftest bash-completion-tokenize-test ()
  (should (equal '("a" "hello" "world" "b" "c")
		 (sz-testutils-with-buffer
		  '("a hello world b c")
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; extra spaces
  (should (equal '("a" "hello" "world" "b" "c")
  		 (sz-testutils-with-buffer
  		  '("  a  hello \n world \t b \r c  ")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position 2))))))

  ;; escaped spaces
  (should (equal '("a" "hello world" "b" "c")
  		 (sz-testutils-with-buffer
  		  '("a hello\\ world b c")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; escaped #
  (should (equal '("a" "hello" "#world#" "b")
  		 (sz-testutils-with-buffer
  		  '("a hello \\#world\\# b")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; double quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (sz-testutils-with-buffer
  		  '("a \"hello world\" b c")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; escaped double quotes
  (should (equal '("a" "-\"hello world\"-" "b" "c")
  		 (sz-testutils-with-buffer
  		  '("a \"-\\\"hello world\\\"-\" b c")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; single quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (sz-testutils-with-buffer
  		  '("a \"hello world\" b c")
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))
  
  ;; escaped single quotes
  (should (equal '("a" "-'hello world'-" "b" "c")
		 (sz-testutils-with-buffer
		  '("a '-\\'hello world\\'-' b c")
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; complex quote mix
  (should (equal '("a" "hello world bc" "d")
		 (sz-testutils-with-buffer
		  '("a hel\"lo w\"o'rld b'c d")
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; unescaped semicolon
  (should (equal '("to" "infinity" ";" "and beyond")
  		 (sz-testutils-with-buffer
  		  "to infinity;and\\ beyond"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; unescaped &&"
  (should (equal '("to" "infinity" "&&" "and beyond")
		 (sz-testutils-with-buffer
		  "to infinity&&and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;;unescaped ||"
  (should (equal '("to" "infinity" "||" "and beyond")
		 (sz-testutils-with-buffer
		  "to infinity||and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; quoted ;&|"
  (should (equal '("to" "infinity;&|and" "beyond")
		 (sz-testutils-with-buffer
  		  "to \"infinity;&|and\" beyond"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position)))))))

(ert-deftest bash-completion-process-tokens-test ()
  ;; cursor at end of word
  (should (equal
	   '((line . "a hello world")
	     (point . 13)
	     (cword . 2)
	     (words . ("a" "hello" "world"))
	     (stub-start . 9))
	   (sz-testutils-with-buffer
	    "a hello world"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 14 nil))))

  ;; just one space, cursor after it
  (should (equal
	   '((line . "")
	     (point . 0)
	     (cword . 0)
	     (words . (""))
	     (stub-start . 2))
	   (sz-testutils-with-buffer
	    " "
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 2 nil))))

  ;; some words separated by spaces, cursor after the last space
  (should (equal
	   '((line . "a hello ")
	     (point . 8)
	     (cword . 2)
	     (words . ("a" "hello" ""))
	     (stub-start . 9))
	   (sz-testutils-with-buffer
	    "a hello "
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 9 nil))))
  
  ;; complex multi-command line
  (should (equal 
	   '((line . "make -")
	     (point . 6)
	     (cword . 1)
	     (words . ("make" "-"))
	     (stub-start . 27))
	   (sz-testutils-with-buffer
	    "cd /var/tmp ; ZORG=t make -"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 28 nil))))

  ;; pipe
  (should (equal 
	   '((line . "sort -")
	     (point . 6)
	     (cword . 1)
	     (words . ("sort" "-"))
	     (stub-start . 20))
	   (sz-testutils-with-buffer
	    "ls /var/tmp | sort -"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 21 nil))))

  ;; escaped semicolon
  (should (equal 
	   '((line . "find -name '*.txt' -exec echo {} ';' -")
	     (point . 38)
	     (cword . 7)
	     (words . ("find" "-name" "*.txt" "-exec" "echo" "{}" ";" "-"))
	     (stub-start . 38))
	   (sz-testutils-with-buffer
	    "find -name '*.txt' -exec echo {} ';' -"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 39 nil))))

  ;; at var assignment
  (should (equal 
	   '((line . "ZORG=t")
	     (point . 6)
	     (cword . 0)
	     (words . ("ZORG=t"))
	     (stub-start . 19))
	   (sz-testutils-with-buffer
	    "cd /var/tmp ; A=f ZORG=t"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 25 nil))))

  ;; with escaped quote
  (should (equal 
	   '((line . "cd /vcr/shows/Dexter\\'s")
	     (point . 23)
	     (cword . 1)
	     (words . ("cd" "/vcr/shows/Dexter's"))
	     (stub-start . 4))
	   (sz-testutils-with-buffer
	    "cd /vcr/shows/Dexter\\'s"
	    (bash-completion-process-tokens
	     (bash-completion-tokenize (point-min) (point-max)) 24 nil)))))

(ert-deftest bash-completion-add-to-alist-test ()
  ;; garbage
  (should (equal nil
		 (let ((bash-completion-alist nil))
		   (bash-completion-add-to-alist (list "just" "some" "garbage")))))

  ;; empty
  (should (equal nil
		 (let ((bash-completion-alist nil))
		   (bash-completion-add-to-alist '()))))

  ;; empty string
  (should (equal nil
		 (let ((bash-completion-alist nil))
		   (bash-completion-add-to-alist (list "")))))

  ;; empty complete
  (should (equal  nil
		  (let ((bash-completion-alist nil))
		    (bash-completion-add-to-alist (list "complete")))))

  ;; one command
  (should (equal '(("cdb" . ("-e" "-F" "_cdargs_aliases")))
		 (let (bash-completion-alist)
		   (bash-completion-add-to-alist
		    (list "complete" "-e" "-F" "_cdargs_aliases" "cdb"))))))

(ert-deftest bash-completion-build-alist-test ()
  (should (equal
	   '(("cdb" "-F" "_cdargs_aliases")
	     ("project" "-F" "complete_projects")
	     ("pro" "-F" "complete_projects")
	     ("cv" "-F" "_cdargs_aliases")
	     ("cb" "-F" "_cdargs_aliases")
	     (nil "-F" "_completion_loader"))
	   (sz-testutils-with-buffer
	    "
complete -F _cdargs_aliases cdb
complete -F complete_projects project
complete -F complete_projects pro
complete -F _cdargs_aliases cv
complete -F _cdargs_aliases cb
complete -F _completion_loader -D
garbage
"
	    (let ((bash-completion-alist '(garbage)))
	      (bash-completion-build-alist (current-buffer)))))))

(ert-deftest bash-completion-quote-test ()
  ;; not necessary
  (should (equal "hello"
		 (bash-completion-quote "hello")))
  ;; space"
  (should (equal "'hello world'"
		 (bash-completion-quote "hello world")))

  ;; quote
  (should (equal "'hell'\\''o'"
		 (bash-completion-quote "hell'o"))))

(ert-deftest bash-completion-generate-list-test ()
  ;; no custom completion
  (should
   (equal (concat "cd >/dev/null 2>&1 " (expand-file-name "~/test")
		  " ; compgen -o default worl 2>/dev/null")
	  (let ((bash-completion-alist nil)
		(default-directory "~/test"))
	    (bash-completion-generate-line "hello worl" 7 '("hello" "worl") 1 nil))))

  ;; custom completion no function or command
  (should (equal 
	   "cd >/dev/null 2>&1 /test ; compgen -A -G '*.txt' -- worl 2>/dev/null"
	   (let ((bash-completion-alist '(("zorg" . ("-A" "-G" "*.txt"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; custom completion function
  (should (equal 
	   "cd >/dev/null 2>&1 /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -F __bash_complete_wrapper -- worl 2>/dev/null"
	   (let ((bash-completion-alist '(("zorg" . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; custom completion command
  (should (equal 
	   "cd >/dev/null 2>&1 /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -F __bash_complete_wrapper -- worl 2>/dev/null"
	   (let ((bash-completion-alist '(("zorg" . ("-C" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; default completion function
  (should (equal 
	   "cd >/dev/null 2>&1 /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -F __bash_complete_wrapper -- worl 2>/dev/null"
	   (let ((bash-completion-alist '((nil . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 t))))

  ;; ignore completion function
  (should (equal 
	   "cd >/dev/null 2>&1 /test ; compgen -o default worl 2>/dev/null"
	   (let ((bash-completion-alist '((nil . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil)))))

(ert-deftest bash-completion-generate-list-test ()
  ;; empty str
  (should (equal nil
		 (bash-completion-starts-with "" "prefix")))

  ;; starts with
  (should (equal t
		 (bash-completion-starts-with "blah-blah" "blah-")))

  ;; does not starts with
  (should (equal nil
		 (bash-completion-starts-with "blah-blah" "blih-")))

  ;; same
  (should (equal t
		 (bash-completion-starts-with "blah-" "blah-"))))

(ert-deftest bash-completion-send-test ()
  (should (equal 
	   (cons 0 "line1\nline2\n")
	   (bash-completion-test-send "line1\nline2\n\t0\v")))

  ;; command failed"
  (should (equal 
	   (cons 1 "line1\nline2\n")
	   (bash-completion-test-send "line1\nline2\n\t1\v")))

  ;; wrapped function returned 124"
  (should (equal 
	   (cons 124 "line1\nline2\n")
	   (bash-completion-test-send
	    (concat "line1\nli" bash-completion-wrapped-status "ne2\n\t0\v")))))

(ert-deftest bash-completion-cd-command-prefix-test ()
  ;; no current dir
  (should (equal ""
		 (let ((default-directory nil))
		   (bash-completion-cd-command-prefix))))
  
  ;; current dir
  (should (equal "cd >/dev/null 2>&1 /tmp/x ; "
		 (let ((default-directory "/tmp/x"))
		   (bash-completion-cd-command-prefix))))

  ;; expand tilde
  (should (equal
	   (concat "cd >/dev/null 2>&1 " (expand-file-name "~/x") " ; ")
	   (let ((default-directory "~/x"))
	     (bash-completion-cd-command-prefix)))))

(ert-deftest bash-completion-addsuffix-test ()
  (cl-letf (((symbol-function 'file-accessible-directory-p)
	     (lambda (a) (error "unexpected"))))
    (should (equal "hello/" (bash-completion-addsuffix "hello/")))
    ;; ends with space"
    (should (equal "hello " (bash-completion-addsuffix "hello ")))
    ;; ends with separator"
    (should (equal "hello:" (bash-completion-addsuffix "hello:"))))
  ;; check directory"
  (cl-letf (((symbol-function 'file-accessible-directory-p)
	     (lambda (a) (equal a "/tmp/hello")))
	    (default-directory "/tmp"))
    (should (equal "hello/" (bash-completion-addsuffix "hello"))))
  (cl-letf (((symbol-function 'file-accessible-directory-p)
	     (lambda (a) (equal a (concat (expand-file-name "y" "~/x")))))
	    (default-directory "~/x"))
    (should (equal "y/" (bash-completion-addsuffix "y")))))

(ert-deftest bash-completion-starts-with-test ()
  (should (equal nil (bash-completion-starts-with "" "hello ")))
  (should (equal t (bash-completion-starts-with "hello world" "hello ")))
  (should (equal nil (bash-completion-starts-with "hello world" "hullo ")))
  (should (equal t (bash-completion-starts-with "hello" ""))))

(ert-deftest bash-completion-ends-with-test ()
  (should (equal nil (bash-completion-ends-with "" "world")))
  (should (equal t (bash-completion-ends-with "hello world" "world")))
  (should (equal nil (bash-completion-ends-with "hello world" "wurld")))
  (should (equal t (bash-completion-ends-with "hello" ""))))

(ert-deftest bash-completion-last-wordbreak-test ()
  (should (equal '("a:b:c:d:" . "e")
		 (bash-completion-last-wordbreak-split "a:b:c:d:e")))
  (should (equal '("hello=" . "world")
		 (bash-completion-last-wordbreak-split "hello=world")))
  (should (equal '("hello>" . "world")
		 (bash-completion-last-wordbreak-split "hello>world")))
  (should (equal '(">" . "world")
		 (bash-completion-last-wordbreak-split ">world")))
  (should (equal '("" . "hello")
		 (bash-completion-last-wordbreak-split "hello"))))

(ert-deftest bash-completion-before-last-wordbreak-test ()
  (should (equal "a:b:c:d:"
		 (bash-completion-before-last-wordbreak "a:b:c:d:e")))
  (should (equal "hello="
		 (bash-completion-before-last-wordbreak "hello=world")))
  (should (equal "hello>"
		 (bash-completion-before-last-wordbreak "hello>world")))
  (should (equal "" (bash-completion-before-last-wordbreak "hello"))))

(ert-deftest bash-completion-after-last-wordbreak-test ()
  (should (equal "e"
		 (bash-completion-after-last-wordbreak "a:b:c:d:e")))
  (should (equal "world"
		 (bash-completion-after-last-wordbreak "hello=world")))
  (should (equal "world"
		 (bash-completion-after-last-wordbreak "hello>world")))
  (should (equal "hello"
		 (bash-completion-after-last-wordbreak "hello"))))

(ert-deftest bash-completion-fix-test ()
  ;; escape rest
  (should (equal "a\\ bc\\ d\\ e"
		 (bash-completion-fix "a\\ bc d e" "a\\ b")))

  ;; do not escape final space
  (should (equal "ab "
		 (let ((bash-completion-nospace nil))
		   (bash-completion-fix "ab " "a"))))
  
  ;; remove final space
  (should (equal "ab"
		 (let ((bash-completion-nospace t))
		   (bash-completion-fix "ab " "a"))))

  ;; unexpand home and escape
  (should (equal "~/a/hello\\ world"
		 (bash-completion-fix (expand-file-name "~/a/hello world")
				      "~/a/he")))

  ;; match after wordbreak and escape
  (should (equal "a:b:c:hello\\ world"
		 (bash-completion-fix "hello world" "a:b:c:he")))

  ;; just append
  (should (equal "hello\\ world"
		 (bash-completion-fix " world" "hello")))

  ;; subset of the prefix"
  (should (equal "Dexter"
		 (bash-completion-fix "Dexter" "Dexter'"))))

(ert-deftest bash-completion-extract-candidates-test ()
  (should (equal 
	   '("hello\\ world" "hello ")
	   (sz-testutils-with-buffer
	    "hello world\nhello \n\n"
	    (cl-letf (((symbol-function 'bash-completion-buffer)
		       (lambda () (current-buffer)))
		      (bash-completion-nospace nil))
	      (bash-completion-extract-candidates "hello" nil))))))

(ert-deftest bash-completion-nonsep-test ()
  (should (equal "^ \t\n\r;&|'\"#"
		 (bash-completion-nonsep nil)))
  (should (equal "^ \t\n\r'"
		 (bash-completion-nonsep ?')))
  (should (equal "^ \t\n\r\""
		 (bash-completion-nonsep ?\"))))

(ert-deftest bash-completion-escape-candidate-test ()
  ;; empty string - no quote
  (should (equal ""
		 (bash-completion-escape-candidate "" nil)))

  ;; empty string - single quote
  (should (equal ""
		 (bash-completion-escape-candidate "" ?')))

  ;; empty string - no quote
  (should (equal ""
		 (bash-completion-escape-candidate "" ?\")))

  ;; no quote
  (should (equal "He\\ said\\:\\ \\\"hello\\,\\ \\'you\\'\\\""
		 (bash-completion-escape-candidate "He said: \"hello, 'you'\"" nil)))

  ;; no quote
  (should (equal "\\#hello\\#"
		 (bash-completion-escape-candidate "#hello#" nil)))

  ;; no quote
  (should (equal "\\(hello\\)"
		 (bash-completion-escape-candidate "(hello)" nil)))

  ;; single quote
  (should (equal "He said: \"hello, '\\''you'\\''\""
		 (bash-completion-escape-candidate "He said: \"hello, 'you'\"" ?')))

  ;; double quote
  (should (equal "He said: \\\"hello, 'you'\\\""
		 (bash-completion-escape-candidate "He said: \"hello, 'you'\"" ?\")))

  ;; double quote: specials
  (should (equal "Specials in double quotes: \\$\\`\\\""
		 (bash-completion-escape-candidate "Specials in double quotes: $`\"" ?\")))
  
  ;; double quote: escaped specials
  (should (equal "Slash-prefixed specials in double quotes: \\\\\\$\\\\\\`\\\\\\\""
		 (bash-completion-escape-candidate "Slash-prefixed specials in double quotes: \\$\\`\\\"" ?\")))

  ;; double quote: do not escape backslash which does not escape anything
  (should (equal "hel\\lo, \\you"
		 (bash-completion-escape-candidate "hel\\lo, \\you" ?\")))

  ;; no quote not if double quoted
  (should (equal "\"hello, you"
		 (bash-completion-escape-candidate "\"hello, you" nil)))
  
  ;; no quote not if single quoted
  (should (equal "'hello, you"
		 (bash-completion-escape-candidate "'hello, you" nil))))

(ert-deftest bash-completion-quote-test ()
  ;; allowed
  (should (equal "abc_ABC/1-2.3"
		 (bash-completion-quote "abc_ABC/1-2.3")))

  ;; quoted
  (should (equal "'a$b'" (bash-completion-quote "a$b")))

  ;; quoted single quote
  (should (equal "'a'\\''b'" (bash-completion-quote "a'b"))))

(ert-deftest bash-completion-join-test ()
  (should (equal "ls -l /a/b '/a/b c' '/a/b'\\''c' '$help/d'"
		 (bash-completion-join '("ls" "-l" "/a/b" "/a/b c" "/a/b'c" "$help/d")))))


;; ---------- integration tests

(defmacro bash-completion_test-harness (&rest body)
  `(let ((bash-completion-process nil) (bash-completion-alist nil))
     (unwind-protect
	 (progn ,@body)
       ;; tearDown
       (condition-case err
	   (when bash-completion-process
	     (let ((buffer (process-buffer bash-completion-process)))
	       (kill-process bash-completion-process)
	       (sit-for 0.2)
	       (kill-buffer buffer)))
	 (error (message "error in bash-completion_test tearDown: %s" err))))))

(defmacro bash-completion_test-with-shell (complete-me)
  `(bash-completion_test-harness
    (let ((explicit-shell-file-name bash-completion-prog)
	  shell-buffer)
      (unwind-protect
	  (progn
	    (setq shell-buffer (shell (generate-new-buffer-name
				       "*bash-completion_test-with-shell*")))
	    ;; accept process output until there's nothing left
	    (while (accept-process-output nil 0.6))
	    ;; do a completion and return the result
	    (with-current-buffer shell-buffer
	      (insert ,complete-me)
	      (if bash-completion-comint-uses-standard-completion
		  (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete)))
		    (completion-at-point))
		(bash-completion-dynamic-complete))
	      (buffer-substring (comint-line-beginning-position) (point))))
	;; finally
	(when (and shell-buffer (buffer-live-p shell-buffer))
	  (kill-process (get-buffer-process shell-buffer))
	  (sit-for 0.2)
	  (kill-buffer shell-buffer))))))

(ert-deftest bash-completion-interaction-test ()
  (skip-unless (file-executable-p bash-completion-prog))
  (bash-completion_test-harness
   (should-not (bash-completion-is-running))
   (should (buffer-live-p (bash-completion-buffer)))
   (should (bash-completion-is-running))
   (should-not (null (member
		      "help "
		      (let ((bash-completion-nospace nil))
			(bash-completion-comm "hel" 4 '("hel") 0 nil)))))
   (bash-completion-reset)
   (should-not (bash-completion-is-running))))

(ert-deftest bash-completion-setenv-test ()
  (skip-unless (file-executable-p bash-completion-prog))
  (bash-completion_test-harness
   (bash-completion-send "echo $EMACS_BASH_COMPLETE")
   (with-current-buffer (bash-completion-buffer)
     (should (equal "t\n" (buffer-string))))))

(ert-deftest bash-completion-one-completion-test ()
  (skip-unless (file-executable-p bash-completion-prog))
  (should (equal "__bash_complete_wrapper "
		 (let ((bash-completion-nospace nil))
		   (bash-completion_test-with-shell "__bash_complete_")))))

(ert-deftest bash-completion-wordbreak-completion-test ()
  (skip-unless (file-executable-p bash-completion-prog))
  (should (equal "export PATH=/sbin:/bin/"
		 (bash-completion_test-with-shell "export PATH=/sbin:/bi"))))

;;; bash-completion_test.el ends here
