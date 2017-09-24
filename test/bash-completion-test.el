;;; bash-completion-test.el --- Tests bash-completion.el

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
;; `bash-completion' that don't rely on a bash process.
;;

;;; History:
;;

;;; Code:
(require 'bash-completion)
(require 'cl)
(if (>= emacs-major-version 24)
    (require 'ert)
  (require 'pre24))

(defmacro bash-completion-test-with-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.

The return value is the one returned by BODY."
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (progn ,@body)))


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
		 (bash-completion-test-with-buffer
		  "a hello world b c"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; extra spaces
  (should (equal '("a" "hello" "world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "  a  hello \n world \t b \r c  "
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position 2))))))

  ;; escaped spaces
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a hello\\ world b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; escaped #
  (should (equal '("a" "hello" "#world#" "b")
  		 (bash-completion-test-with-buffer
  		  "a hello \\#world\\# b"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; double quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"hello world\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; escaped double quotes
  (should (equal '("a" "-\"hello world\"-" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"-\\\"hello world\\\"-\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; single quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"hello world\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))
  
  ;; escaped single quotes
  (should (equal '("a" "-'hello world'-" "b" "c")
		 (bash-completion-test-with-buffer
		  "a '-\\'hello world\\'-' b c"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; complex quote mix
  (should (equal '("a" "hello world bc" "d")
		 (bash-completion-test-with-buffer
		  "a hel\"lo w\"o'rld b'c d"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; unescaped semicolon
  (should (equal '("to" "infinity" ";" "and beyond")
  		 (bash-completion-test-with-buffer
  		  "to infinity;and\\ beyond"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; unescaped &&"
  (should (equal '("to" "infinity" "&&" "and beyond")
		 (bash-completion-test-with-buffer
		  "to infinity&&and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;;unescaped ||"
  (should (equal '("to" "infinity" "||" "and beyond")
		 (bash-completion-test-with-buffer
		  "to infinity||and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (line-end-position))))))

  ;; quoted ;&|"
  (should (equal '("to" "infinity;&|and" "beyond")
		 (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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
	   (bash-completion-test-with-buffer
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

(ert-deftest bash-completion-generate-line-test ()
  ;; no custom completion
  (should
   (equal (cons 'default
                (concat "cd >/dev/null 2>&1 " (expand-file-name "~/test")
                        " ; compgen -o default -- worl 2>/dev/null"))
	  (let ((bash-completion-alist nil)
		(default-directory "~/test"))
	    (bash-completion-generate-line "hello worl" 7 '("hello" "worl") 1 nil))))

  ;; custom completion no function or command
  (should (equal
           (cons 'custom
                 "cd >/dev/null 2>&1 /test ; compgen -A -G '*.txt' -- worl 2>/dev/null")
	   (let ((bash-completion-alist '(("zorg" . ("-A" "-G" "*.txt"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; custom completion function
  (should (equal
           (cons 'custom
                 (concat
                  "cd >/dev/null 2>&1 /test ; "
                  "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; "
                  "COMP_POINT=7; COMP_CWORD=1; "
                  "COMP_WORDS=( zorg worl ); "
                  "__zorg \"${COMP_WORDS[@]}\"' "
                  "compgen -F __bash_complete_wrapper -- worl 2>/dev/null"))
	   (let ((bash-completion-alist '(("zorg" . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; custom completion command
  (should (equal
           (cons 'custom
                 (concat
                  "cd >/dev/null 2>&1 /test ; "
                  "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; "
                  "COMP_POINT=7; "
                  "COMP_CWORD=1; "
                  "COMP_WORDS=( zorg worl ); "
                  "__zorg \"${COMP_WORDS[@]}\"' "
                  "compgen -F __bash_complete_wrapper -- worl 2>/dev/null"))
	   (let ((bash-completion-alist '(("zorg" . ("-C" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil))))

  ;; default completion function
  (should (equal
           (cons 'custom
                 (concat
                  "cd >/dev/null 2>&1 /test ; "
                  "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; "
                  "COMP_POINT=7; "
                  "COMP_CWORD=1; "
                  "COMP_WORDS=( zorg worl ); "
                  "__zorg \"${COMP_WORDS[@]}\"' "
                  "compgen -F __bash_complete_wrapper -- worl 2>/dev/null"))
	   (let ((bash-completion-alist '((nil . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 t))))

  ;; ignore completion function
  (should (equal
           (cons 'default
                 "cd >/dev/null 2>&1 /test ; compgen -o default -- worl 2>/dev/null")
	   (let ((bash-completion-alist '((nil . ("-F" "__zorg"))))
		 (default-directory "/test"))
	     (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 nil)))))

(ert-deftest bash-completion--find-last-test ()
  (should (equal nil (bash-completion--find-last ?a "xxxxx")))
  (should (equal 3 (bash-completion--find-last ?d "abcdef")))
  (should (equal 5 (bash-completion--find-last ?f "abcdef")))
  (should (equal 9 (bash-completion--find-last ?d "abcdefabcdef"))))

(ert-deftest bash-completion-starts-with-test ()
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
      (bash-completion-test-with-buffer
       ""
       (cons
        (bash-completion-send "cmd" 'process 3.14)
        (buffer-string))))))

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

(ert-deftest bash-completion-starts-with-test ()
  (should (equal nil (bash-completion-starts-with "" "hello ")))
  (should (equal t (bash-completion-starts-with "hello world" "hello ")))
  (should (equal nil (bash-completion-starts-with "hello world" "hullo ")))
  (should (equal t (bash-completion-starts-with "hello" ""))))

(ert-deftest bash-completion-last-wordbreak-test ()
  (should (equal '("a:b:c:d:" "e" ?:)
		 (bash-completion-last-wordbreak-split "a:b:c:d:e")))
  (should (equal '("hello=" "world" ?=)
		 (bash-completion-last-wordbreak-split "hello=world")))
  (should (equal '("hello>" "world" ?>)
		 (bash-completion-last-wordbreak-split "hello>world")))
  (should (equal '(">" "world" ?>)
		 (bash-completion-last-wordbreak-split ">world")))
  (should (equal '("" "hello" ?\0)
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
		 (bash-completion-fix "a\\ bc d e" "a\\ b" "a\\ b" nil nil nil)))

  ;; recover original escaping
  (should (equal "a' 'bc\\ d\\ e"
		 (bash-completion-fix "a\\ bc d e" "a\\ b" "a' 'b" nil nil nil)))

  ;; do not escape final space
  (should (equal "ab "
		 (let ((bash-completion-nospace nil))
		   (bash-completion-fix "ab " "a" "a" nil nil nil))))
  
  ;; remove final space
  (should (equal "ab"
		 (let ((bash-completion-nospace t))
		   (bash-completion-fix "ab " "a" "a" nil nil nil))))

  ;; unexpand home and escape
  (should (equal "~/a/hello\\ world"
		 (bash-completion-fix (expand-file-name "~/a/hello world")
				      "~/a/he" "~/a/he" nil nil nil)))

  ;; match after wordbreak and escape
  (should (equal "a:b:c:hello\\ world"
		 (bash-completion-fix "hello world" "a:b:c:he" "a:b:c:he"
                                      nil nil nil)))

  ;; just append
  (should (equal "hello\\ world"
		 (bash-completion-fix " world" "hello" "hello" nil nil nil)))

  ;; append / for home
  (should (equal "~/"
                 (bash-completion-fix (expand-file-name "~")
                                      "~" "~" nil 'default nil)))

  (cl-letf (((symbol-function 'file-accessible-directory-p)
             (lambda (d) (equal d "/tmp/somedir"))))
    (let ((default-directory "/tmp/"))
      ;; append / for directory
      (should (equal "somedir/"
                     (bash-completion-fix "somedir" "some" "some"
                                          nil 'default nil)))
      ;; append / for initial command that is a directory
      (should (equal "somedir/"
                     (bash-completion-fix "somedir" "some" "some"
                                          nil 'command nil)))))

  ;; append a space for initial command that is not a directory
  (should (let ((bash-completion-nospace nil))
            (equal "somecmd "
                   (bash-completion-fix "somecmd" "some" "some"
                                        nil 'command nil))))

  ;; ... but not if nospace is t.
  (should (let ((bash-completion-nospace t))
            (equal "somecmd"
                   (bash-completion-fix "somecmd" "some" "some"
                                        nil 'command nil))))

  ;; append a space for a single default completion
  (should (let ((bash-completion-nospace nil))
            (equal "somecmd "
                   (bash-completion-fix "somecmd" "some" "some"
                                        nil 'default 'single))))

  ;; but only for a single completion
  (should (let ((bash-completion-nospace nil))
            (equal "somecmd"
                   (bash-completion-fix "somecmd" "some" "some"
                                        nil 'default nil))))

  ;; subset of the prefix"
  (should (equal "Dexter"
		 (bash-completion-fix "Dexter" "Dexter'" "Dexter'"
                                      nil nil nil))))

(ert-deftest bash-completion-extract-candidates-test ()
  (let ((bash-completion-nospace nil))
    (should
     (equal
      '("hello\\ world" "hello ")
      (bash-completion-test-with-buffer
       "hello world\nhello \n\n"
       (cl-letf (((symbol-function 'bash-completion-buffer)
                  (lambda () (current-buffer))))
         (bash-completion-extract-candidates "hello" "hello" nil nil)))))))

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

  ;; no quote
  (should (equal "--hello="
		 (bash-completion-escape-candidate "--hello=" nil)))

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

  ;; starts with quotes or special char
  (should (equal "\\\"\\\"hello" (bash-completion-escape-candidate "\"\"hello" nil)))
  (should (equal "\\'\\'hello" (bash-completion-escape-candidate "''hello" nil)))
  (should (equal "\\#\\#hello" (bash-completion-escape-candidate "##hello" nil)))
  (should (equal "\\\"\\\"hello" (bash-completion-escape-candidate "\"\"hello" ?\")))
  (should (equal "''hello" (bash-completion-escape-candidate "''hello" ?\")))
  (should (equal "##hello" (bash-completion-escape-candidate "##hello" ?\")))
  (should (equal "\"\"hello" (bash-completion-escape-candidate "\"\"hello" ?')))
  (should (equal "'\\'''\\''hello" (bash-completion-escape-candidate "''hello" ?')))
  (should (equal "##hello" (bash-completion-escape-candidate "##hello" ?'))))

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

(defmacro --with-fake-bash-completion-send (&rest body)
  "Runs the body in an environment that fakes `bash-completion-send'.

When `bash-completion-send' is called, it pops the result from
--send-results and captures the command-line it was given into
--captured-commands.

Directories in --directories get a / appended to them. Note that
the current directory in this environemnt is /tmp/test.

The body is run with a test buffer as current buffer. Fill it with the command-line
before calling `bash-completion-dynamic-complete-nocomint'.
"
  `(let ((default-directory "/tmp/test")
         (bash-completion-alist '()))
     (lexical-let ((--process-buffer)
                   (--test-buffer)
                   (--send-results (list))
                   (--captured-commands (list))
                   (--directories (list)))
       (with-temp-buffer
         (setq --process-buffer (current-buffer))
         (with-temp-buffer
           (setq --test-buffer (current-buffer))
           (cl-letf (((symbol-function 'bash-completion-require-process) (lambda () 'process))
                     ((symbol-function 'bash-completion-buffer) (lambda () --process-buffer))
                     ((symbol-function 'process-buffer) (lambda (p) --process-buffer))
                     ((symbol-function 'file-accessible-directory-p)
                      (lambda (d) (member d --directories)))
                     ((symbol-function 'bash-completion-send)
                      (lambda (commandline &optional process timeout)
                        (with-current-buffer --process-buffer
                          (delete-region (point-min) (point-max))
                          (insert (pop --send-results))
                          (push commandline --captured-commands)
                          0))))
             (progn ,@body)))))))

(ert-deftest bash-completion-simple-complete-test ()
  (--with-fake-bash-completion-send
   (push "hell\nhello1\nhello2\n" --send-results)
   (insert "$ cat he")
   (should (equal
            (list 7 9 '("hell" "hello1" "hello2"))
            (bash-completion-dynamic-complete-nocomint 3 (point))))
   (should (equal "cd >/dev/null 2>&1 /tmp/test ; compgen -o default -- he 2>/dev/null"
                  (pop --captured-commands)))))

(ert-deftest bash-completion-single-completion-test ()
  (--with-fake-bash-completion-send
   (push "hello\n" --send-results)
   (insert "$ cat he")
   (should (equal
            '("hello ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))


(ert-deftest bash-completion-single-completion-double-quotes ()
  (--with-fake-bash-completion-send
   (push "hello\n" --send-results)
   (insert "$ cat \"he")
   (should (equal
            '("hello\" ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-single-completion-single-quotes ()
  (--with-fake-bash-completion-send
   (push "hello\n" --send-results)
   (insert "$ cat 'he")
   (should (equal
            '("hello' ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-completion-with-double-quotes ()
  (--with-fake-bash-completion-send
   (push "hell\nhello\n" --send-results)
   (insert "$ cat \"he")
   (should (equal
            '("hell\"" "hello\"")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-trailing-default-completion ()
  (--with-fake-bash-completion-send
   (push "without space\nwith space \nwith slash/\n" --send-results)
   (insert "$ ls with")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("without\\ space" "with\\ space " "with\\ slash/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-trailing-default-completion-nospace ()
  (--with-fake-bash-completion-send
   (push "without space\nwith space \nwith slash/\n" --send-results)
   (insert "$ ls with")
   (let ((bash-completion-nospace t))
     (should (equal
              '("without\\ space" "with\\ space" "with\\ slash/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-trailing-custom-completion ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "without space\nwith space \nwith slash/\n" --send-results)
   (insert "$ ls with")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("without\\ space" "with\\ space " "with\\ slash/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-trailing-custom-completion-nospace ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "without space\nwith space \nwith slash/\n" --send-results)
   (insert "$ ls with")
   (let ((bash-completion-nospace t))
     (should (equal
              '("without\\ space" "with\\ space" "with\\ slash/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-trailing-custom-flag-completion ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "--color\n--color=\n" --send-results)
   (insert "$ ls --c")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("--color" "--color=")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-complete-dir-with-spaces-test ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/Documents" --directories)
   (push "/tmp/test/Documents/Modes d'emplois" --directories)
   (push "/tmp/test/Documents\n" --send-results)
   (push "Documents\n" --send-results)
   (insert "$ cat Doc")
   (should (equal
            '(7 10 ("Documents/"))
            (bash-completion-dynamic-complete-nocomint 3 (point))))
   (insert "uments/")
   (push "Documents/Modes d'emplois\n" --send-results)
   (should (equal
            '("Documents/Modes\\ d\\'emplois/")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))
   (insert "Modes\\ d\\'emplois/")
   (push "Documents/Modes d'emplois/KAR 1.pdf\nDocuments/Modes d'emplois/KAR 2.pdf\n"
         --send-results)
   (should (equal
            '("Documents/Modes\\ d\\'emplois/KAR\\ 1.pdf"
              "Documents/Modes\\ d\\'emplois/KAR\\ 2.pdf")
            (nth 2(bash-completion-dynamic-complete-nocomint 3 (point)))))
   (should (equal
            (concat
             "cd >/dev/null 2>&1 /tmp/test ; "
             "compgen -o default -- 'Documents/Modes d'\\''emplois/' 2>/dev/null")
            (pop --captured-commands)))))

(ert-deftest bash-completion-complete-single-quoted-dir ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/Documents" --directories)
   (push "/tmp/test/Documents/Modes d'emplois" --directories)
   (push "/tmp/test/Documents\n" --send-results)
   (push "Documents\n" --send-results)
   (insert "$ cat 'Doc")
   (should (equal
            '(8 11 ("Documents/"))
            (bash-completion-dynamic-complete-nocomint 3 (point))))
   (insert "uments/")
   (push "Documents/Modes d'emplois\n" --send-results)
   (should (equal
            '("Documents/Modes d'\\''emplois/")
            (nth 2(bash-completion-dynamic-complete-nocomint 3 (point)))))
   (insert "Modes d'\\''emplois/")
   (push "Documents/Modes d'emplois/KAR 1.pdf\nDocuments/Modes d'emplois/KAR 2.pdf\n"
         --send-results)
   (should (equal
            '("Documents/Modes d'\\''emplois/KAR 1.pdf'"
              "Documents/Modes d'\\''emplois/KAR 2.pdf'")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-complete-command-with-dir ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/bin" --directories)
   (push "bin\nbind\n" --send-results)
   (insert "$ b")
   (should (equal
            '("bin/" "bind ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))
   (should (equal (concat "cd >/dev/null 2>&1 /tmp/test ; "
                          "compgen -b -c -a -A function -- b 2>/dev/null")
                  (pop --captured-commands)))))

(ert-deftest bash-completion-complete-command-with-space ()
  (--with-fake-bash-completion-send
   (push "some command\n" --send-results)
   (insert "$ some\\ c")
   (should (equal
            '("some\\ command ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))
   (should (equal (concat "cd >/dev/null 2>&1 /tmp/test ; "
                          "compgen -b -c -a -A function -- 'some c' 2>/dev/null")
                  (pop --captured-commands)))))

(ert-deftest bash-completion-failed-completion ()
  (--with-fake-bash-completion-send
   (setq --send-results '("" "bad"))
   (insert "$ ls --")
   (should
    (null (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-wordbreak-completion ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/bin" --directories)
   (setq --send-results '("" "./binary\n./bind\n./bin\n"))
   (insert "$ export PATH=$PATH:./b")
   (should
    (equal '(21 24 ("./binary" "./bind" "./bin/"))
           (bash-completion-dynamic-complete-nocomint 3 (point))))))

(ert-deftest bash-completion-single-wordbreak-completion ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/bin" --directories)
   (setq --send-results '("" "./world\n"))
   (insert "$ set a=./hello:./w")
   (should
    (equal '(17 20 ("./world "))
           (bash-completion-dynamic-complete-nocomint 3 (point))))))

(ert-deftest bash-completion-single-custom-completion ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "--escape\n" --send-results)
   (insert "$ ls --esc")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("--escape ")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-single-custom-completion-with-wordbreak-end ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "--color=\n" --send-results)
   (insert "$ ls --col")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("--color=")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-single-custom-completion-as-directory-explicit ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (push "somedir/\n" --send-results)
   (insert "$ ls some")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("somedir/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-single-custom-completion-as-directory-implicit ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   ;; note that adding a / after a completion is not always the right thing
   ;; to do. See github issue #19.
   (push "/tmp/test/somedir" --directories)
   (push "somedir\n" --send-results)
   (insert "$ ls some")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("somedir/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion-custom-completion-with-fallback ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args")))
   (setq --send-results '("" "foo\nfoobar\n"))
   (insert "$ ls fo")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("foo" "foobar")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))


;;; bash-completion_test.el ends here
