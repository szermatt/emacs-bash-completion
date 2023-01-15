;;; bash-completion-test.el --- Tests bash-completion.el -*- lexical-binding: t -*-
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
(require 'cl-lib)
(require 'ert)

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
		   (bash-completion-tokenize 1 (point-max))))))

  ;; extra spaces and newline
  (should (equal '("a" "hello" "\n" "world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "  a  hello \n world \t b \r c  "
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; escaped spaces
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a hello\\ world b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; double quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"hello world\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; escaped double quotes
  (should (equal '("a" "-\"hello world\"-" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"-\\\"hello world\\\"-\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; single quotes
  (should (equal '("a" "hello world" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"hello world\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; quotes containing newline
  (should (equal '("a" "hello\nworld" "b" "c")
  		 (bash-completion-test-with-buffer
  		  "a \"hello\nworld\" b c"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; escaped single quotes
  (should (equal '("a" "-'hello world'-" "b" "c")
		 (bash-completion-test-with-buffer
		  "a '-\\'hello world\\'-' b c"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (point-max))))))

  ;; complex quote mix
  (should (equal '("a" "hello world bc" "d")
		 (bash-completion-test-with-buffer
		  "a hel\"lo w\"o'rld b'c d"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (point-max))))))

  ;; unescaped semicolon
  (should (equal '("to" "infinity" ";" "and beyond")
  		 (bash-completion-test-with-buffer
  		  "to infinity;and\\ beyond"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max))))))

  ;; unescaped &&"
  (should (equal '("to" "infinity" "&&" "and beyond")
		 (bash-completion-test-with-buffer
		  "to infinity&&and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (point-max))))))

  ;;unescaped ||"
  (should (equal '("to" "infinity" "||" "and beyond")
		 (bash-completion-test-with-buffer
		  "to infinity||and\\ beyond"
		  (bash-completion-strings-from-tokens
		   (bash-completion-tokenize 1 (point-max))))))

  ;; quoted ;&|"
  (should (equal '("to" "infinity;&|and" "beyond")
		 (bash-completion-test-with-buffer
  		  "to \"infinity;&|and\" beyond"
  		  (bash-completion-strings-from-tokens
  		   (bash-completion-tokenize 1 (point-max)))))))

(ert-deftest bash-completion--parse-test ()
  (let ((wordbreaks "@><=;|&(:'\""))
    ;; cursor at end of word
    (should (equal
             (bash-completion--make
              :line "a hello world"
              :cword 2
              :words '("a" "hello" "world")
              :stub-start 9
              :stub "world"
              :unparsed-stub "world"
              :wordbreaks wordbreaks)
	   (bash-completion-test-with-buffer
	    "a hello world"
	    (bash-completion--parse (point-min) 14 wordbreaks))))

  ;; some words separated by spaces, cursor after the last space
  (should (equal
           (bash-completion--make
            :line "a hello "
            :cword 2
            :words '("a" "hello" "")
            :stub-start 9
            :stub ""
            :unparsed-stub ""
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "a hello "
            (bash-completion--parse (point-min) 9 wordbreaks))))

  ;; complex multi-command line
  (should (equal
           (bash-completion--make
            :line "ZORG=t make -"
            :cword 4
            :words '("ZORG" "=" "t" "make" "-")
            :stub-start 27
            :stub "-"
            :unparsed-stub "-"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd /var/tmp ; ZORG=t make -"
            (bash-completion--parse (point-min) 28 wordbreaks))))

  ;; multiple commands on multiple lines
  (should (equal
           (bash-completion--make
            :line "make -"
            :cword 1
            :words '("make" "-")
            :stub-start 18
            :stub "-"
            :unparsed-stub "-"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd /var/tmp\nmake -"
            (bash-completion--parse (point-min) (point-max) wordbreaks))))

  ;; pipe
  (should (equal
           (bash-completion--make
            :line "sort -"
            :cword 1
            :words '("sort" "-")
            :stub-start 20
            :stub "-"
            :unparsed-stub "-"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "ls /var/tmp | sort -"
            (bash-completion--parse (point-min) 21 wordbreaks))))

  ;; escaped semicolon
  (should (equal
           (bash-completion--make
            :line "find -name '*.txt' -exec echo {} ';' -"
            :cword 7
            :words '("find" "-name" "*.txt" "-exec" "echo" "{}" ";" "-")
            :stub-start 38
            :stub "-"
            :unparsed-stub "-"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "find -name '*.txt' -exec echo {} ';' -"
            (bash-completion--parse (point-min) 39 wordbreaks))))

  ;; at var assignment
  (should (equal
           (bash-completion--make
            :line "A=f ZORG=t"
            :cword 5
            :words '("A" "=" "f" "ZORG" "=" "t")
            :stub-start 24
            :stub "t"
            :unparsed-stub "t"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd /var/tmp ; A=f ZORG=t"
            (bash-completion--parse (point-min) 25 wordbreaks))))

  ;; stub is a subset of last word (bash 3)
  (should (equal
           (bash-completion--make
            :line "export PATH=/bin:/usr/bi"
            :cword 5
            :words '("export" "PATH" "=" "/bin" ":" "/usr/bi")
            :stub-start 18
            :stub "/usr/bi"
            :unparsed-stub "/usr/bi"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "export PATH=/bin:/usr/bi"
            (bash-completion--parse (point-min) (point-max) wordbreaks))))

  ;; last word is split according to COMP_WORDBREAKS (bash 4)
  (should (equal
           (bash-completion--make
            :line "export PATH=/bin:/usr/bi"
            :cword 5
            :words '("export" "PATH" "=" "/bin" ":" "/usr/bi")
            :stub-start 18
            :stub "/usr/bi"
            :unparsed-stub "/usr/bi"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "export PATH=/bin:/usr/bi"
            (bash-completion--parse (point-min) (point-max) wordbreaks))))

  ;; with escaped quote
  (should (equal
           (bash-completion--make
            :line "cd /vcr/shows/Dexter's"
            :cword 1
            :words '("cd" "/vcr/shows/Dexter's")
            :stub-start 4
            :stub "/vcr/shows/Dexter's"
            :unparsed-stub "/vcr/shows/Dexter\\'s"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd /vcr/shows/Dexter\\'s"
            (bash-completion--parse (point-min) 24 wordbreaks))))

  ;; with escaped quote, bash 4
  (should (equal
           (bash-completion--make
            :line "cd /vcr/shows/Dexter's"
            :cword 1
            :words '("cd" "/vcr/shows/Dexter's")
            :stub-start 4
            :stub "/vcr/shows/Dexter's"
            :unparsed-stub "/vcr/shows/Dexter\\'s"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd /vcr/shows/Dexter\\'s"
            (bash-completion--parse (point-min) 24 wordbreaks))))

  ;; with double quote
  (should (equal
           (bash-completion--make
            :line "cd /vcr/shows/Dexter's"
            :cword 1
            :words '("cd" "/vcr/shows/Dexter's")
            :stub-start 4
            :stub "/vcr/shows/Dexter's"
            :unparsed-stub "\"/vcr/shows/Dexter's"
            :open-quote ?\"
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd \"/vcr/shows/Dexter's"
            (bash-completion--parse (point-min) 24 wordbreaks))))

  ;; with single quote
  (should (equal
           (bash-completion--make
            :line "cd /vcr/shows/Dexter's"
            :cword 1
            :words '("cd" "/vcr/shows/Dexter's")
            :stub-start 4
            :unparsed-stub "'/vcr/shows/Dexter'\\''s"
            :stub "/vcr/shows/Dexter's"
            :open-quote ?'
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            "cd '/vcr/shows/Dexter'\\''s"
            (bash-completion--parse (point-min) 27 wordbreaks))))

  ;; just one space, cursor after it
  (should (equal
           (bash-completion--make
            :line ""
            :cword 0
            :words '("")
            :stub-start 2
            :stub ""
            :unparsed-stub ""
            :wordbreaks wordbreaks)
           (bash-completion-test-with-buffer
            " "
            (bash-completion--parse (point-min) 2 wordbreaks))))))

(ert-deftest bash-completion-build-alist ()
  (should (equal
	   '(("cdb" "-F" "_cdargs_aliases")
	     ("project" "-F" "complete_projects")
	     ("pro" "-F" "complete_projects")
             ("scp" "-o" "default" "-W" "home\nhome.lan")
	     ("cv" "-F" "_cdargs_aliases")
	     ("cb" "-F" "_cdargs_aliases")
	     (nil "-F" "_completion_loader"))
	   (bash-completion-test-with-buffer
	    "
complete -F _cdargs_aliases cdb
complete -F complete_projects project
complete -F complete_projects pro
complete -o default -W 'home
home.lan' scp
complete -F _cdargs_aliases cv
complete -F _cdargs_aliases cb
complete -F _completion_loader -D
garbage
"
            (bash-completion-build-alist (current-buffer))))))

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
  (let ((default-directory "/test")
        (bash-completion-use-separate-processes t))
    (should
     (equal (concat "cd >/dev/null 2>&1 /test"
                    " && compgen -o default -- worl 2>/dev/null")
            (bash-completion-generate-line
             (bash-completion--make
              :line "hello worl"
              :words '("hello" "worl")
              :stub "worl"
              :unparsed-stub "worl"
              :cword 1))))

    ;; custom completion no function or command
    (should (equal
             "cd >/dev/null 2>&1 /test && compgen -A -G '*.txt' -- worl 2>/dev/null"
             (bash-completion-generate-line
              (bash-completion--make
               :line "zorg worl"
               :words '("zorg" "worl")
               :stub "worl"
               :unparsed-stub "worl"
               :cword 1
               :compgen-args '("-A" "-G" "*.txt")))))

    ;; custom completion function
    (should (equal
             (concat
              "cd >/dev/null 2>&1 /test && "
              "__EMACS_COMPLETE_WRAPPER='COMP_LINE='\\''zorg blah worl'\\''; "
              "COMP_POINT=$(( 1 + ${#COMP_LINE} )); COMP_CWORD=2; "
              "COMP_WORDS=( zorg blah worl ); "
              "__zorg zorg worl blah' "
              "compgen -F __emacs_complete_wrapper -- worl 2>/dev/null")
             (bash-completion-generate-line
              (bash-completion--make
               :line "zorg blah worl"
               :words '("zorg" "blah" "worl")
               :cword 2
               :stub "worl"
               :unparsed-stub "worl"
               :compgen-args '("-F" "__zorg")))))

    ;; custom completion command
    (should (equal
             (concat
              "cd >/dev/null 2>&1 /test && "
              "__EMACS_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; "
              "COMP_POINT=$(( 1 + ${#COMP_LINE} )); "
              "COMP_CWORD=1; "
              "COMP_WORDS=( zorg worl ); "
              "__zorg zorg worl zorg' "
              "compgen -F __emacs_complete_wrapper -- worl 2>/dev/null")
             (bash-completion-generate-line
              (bash-completion--make
               :line "zorg worl"
               :words '("zorg" "worl")
               :cword 1
               :stub "worl"
               :unparsed-stub "worl"
               :compgen-args '("-C" "__zorg")))))

    ;; command completion
    (should (equal
             "cd >/dev/null 2>&1 /test && compgen -b -c -a -A function -- worl 2>/dev/null"
             (bash-completion-generate-line
              (bash-completion--make
               :line "worl"
               :words '("worl")
               :cword 0
               :stub "worl"
               :unparsed-stub "worl"))))))

(ert-deftest bash-completion-customize-test ()
  (cl-letf (((symbol-function 'process-get)
             (lambda (process prop)
               (cond
                ((and (eq 'process process)
                      (eq 'complete-p prop))
                 '((nil "-F" "__default")
                   ("zorg" "-F" "__zorg")))
                ((and (eq 'process process)
                      (eq 'wordbreaks prop)) "\"'@><=;|&(:")
                (t (error "unexpected: (process-get %s %s)"
                          process prop))))))
    (let ((comp (bash-completion--make :cword 1)))
      (setf (bash-completion--words comp) '("zorg" "world"))
      (bash-completion--customize comp 'process)
      (should (equal '("-F" "__zorg") (bash-completion--compgen-args comp)))

      (setf (bash-completion--words comp) '("notzorg" "world"))
      (bash-completion--customize comp 'process)
      (should (equal '("-F" "__default") (bash-completion--compgen-args comp)))

      (bash-completion--customize comp 'process 'nodefault)
      (should (null (bash-completion--compgen-args comp))))))

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
  (let ((process 'process)
        (bash-completion-use-separate-processes t))
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
               (lambda (process timeout &optional millisec just-this-one)
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
	    (concat "line1\nli\e\ewrapped-status=124\e\ene2\n\t0\v")))))

(ert-deftest bash-completion-cd-command-prefix-test ()
  ;; no current dir
  (should (equal ""
		 (let ((default-directory nil))
		   (bash-completion-cd-command-prefix))))

  ;; current dir
  (should (equal "cd >/dev/null 2>&1 /tmp/x && "
		 (let ((default-directory "/tmp/x"))
		   (bash-completion-cd-command-prefix))))

  ;; expand tilde
  (should (equal
	   (concat "cd >/dev/null 2>&1 " (expand-file-name "~/x") " && ")
	   (let ((default-directory "~/x"))
	     (bash-completion-cd-command-prefix)))))

(ert-deftest bash-completion-starts-with-test ()
  (should (equal nil (bash-completion-starts-with "" "hello ")))
  (should (equal t (bash-completion-starts-with "hello world" "hello ")))
  (should (equal nil (bash-completion-starts-with "hello world" "hullo ")))
  (should (equal t (bash-completion-starts-with "hello" ""))))

(ert-deftest bash-completion-last-wordbreak-split-test ()
  (let ((wordbreaks "@><=;|&(:"))
    (should (equal '("a:b:c:d:" "e" ?:)
                   (bash-completion-last-wordbreak-split "a:b:c:d:e" wordbreaks)))
    (should (equal '("hello=" "world" ?=)
                   (bash-completion-last-wordbreak-split "hello=world" wordbreaks)))
    (should (equal '("hello>" "world" ?>)
                   (bash-completion-last-wordbreak-split "hello>world" wordbreaks)))
    (should (equal '(">" "world" ?>)
                   (bash-completion-last-wordbreak-split ">world" wordbreaks)))
    (should (equal '("" "hello" ?\0)
                   (bash-completion-last-wordbreak-split "hello" wordbreaks)))))

(ert-deftest bash-completion-before-last-wordbreak-test ()
  (let ((wordbreaks "@><=;|&(:"))
    (should (equal "a:b:c:d:"
                   (bash-completion-before-last-wordbreak "a:b:c:d:e" wordbreaks)))
    (should (equal "hello="
                   (bash-completion-before-last-wordbreak "hello=world" wordbreaks)))
    (should (equal "hello>"
                   (bash-completion-before-last-wordbreak "hello>world" wordbreaks)))
    (should (equal "" (bash-completion-before-last-wordbreak "hello" wordbreaks)))))

(ert-deftest bash-completion-fix-test ()
  ;; escape rest
  (should (equal "a\\ bc\\ d\\ e"
		 (bash-completion-fix
                  "a\\ bc d e"
                  (bash-completion--make
                   :cword 1
                   :stub "a\\ b"
                   :unparsed-stub "a\\ b"
                   :wordbreaks "")
                  nil)))
                  

  ;; recover original escaping
  (should (equal "a' 'bc\\ d\\ e"
		 (bash-completion-fix
                  "a\\ bc d e"
                  (bash-completion--make
                   :cword 1
                   :stub "a\\ b"
                   :unparsed-stub "a' 'b"
                   :wordbreaks "")
                  nil)))

  ;; do not escape final space
  (should (equal "ab "
                 (bash-completion-fix
                  "ab "
                  (bash-completion--make
                   :cword 1
                   :stub "a"
                   :unparsed-stub "a"
                   :wordbreaks "")
                  nil)))

  ;; remove final space with option nospace
  (should (equal "ab"
                 (bash-completion-fix
                  "ab "
                  (bash-completion--make
                   :cword 1
                   :stub "a"
                   :unparsed-stub "a"
                   :wordbreaks ""
                   :compgen-args '("-o" "nospace"))
                  nil)))

  ;; match after wordbreak and escape
  (should (equal "a:b:c:hello\\ world"
		 (bash-completion-fix
                  "hello world"
                  (bash-completion--make
                   :cword 1
                   :stub "a:b:c:he"
                   :unparsed-stub "a:b:c:he"
                   :wordbreaks "@><=;|&(:")
                  nil)))

  ;; just replace
  (should (equal "something\\ else"
		 (bash-completion-fix
                  "something else"
                  (bash-completion--make
                   :cword 1
                   :stub "something"
                   :unparsed-stub "something"
                   :wordbreaks "")
                  nil)))

  ;; append / for home
  (should (equal "~/"
                 (bash-completion-fix
                  "~"
                  (bash-completion--make
                   :cword 1
                   :stub "~"
                   :unparsed-stub "~"
                   :wordbreaks "")
                  nil)))

  (cl-letf (((symbol-function 'file-accessible-directory-p)
             (lambda (d) (equal d "/tmp/somedir"))))
    (let ((default-directory "/tmp/"))
      ;; append / for directory
      (should (equal "somedir/"
                     (bash-completion-fix
                      "somedir"
                      (bash-completion--make
                       :cword 1
                       :stub "some"
                       :unparsed-stub "some"
                       :wordbreaks ""
                       :compgen-args '(filenames))
                      nil)))))

  ;; append a space for initial command that is not a directory
  (let ((bash-completion-nospace nil))
    (should
     (equal "somecmd "
            (bash-completion-fix
             "somecmd"
             (bash-completion--make
              :cword 0
              :stub "some"
              :unparsed-stub "some"
              :wordbreaks "")
             'single))))

  ;; ... but not if nospace option is set for the function
  (should (equal "somecmd"
                 (bash-completion-fix
                  "somecmd"
                  (bash-completion--make
                   :cword 0
                   :stub "some"
                   :unparsed-stub "some"
                   :wordbreaks ""
                   :compgen-args '("-o" "nospace"))
                  nil)))

  ;; ... or globally
  (let ((bash-completion-nospace t))
    (should (equal "somecmd"
                   (bash-completion-fix
                    "somecmd"
                    (bash-completion--make
                     :cword 0
                     :stub "some"
                     :unparsed-stub "some"
                     :wordbreaks "")
                    nil))))

  ;; append a space for a single completion
  (let ((bash-completion-nospace nil))
    (should (equal "somecmd "
                   (bash-completion-fix
                    "somecmd"
                    (bash-completion--make
                     :cword 0
                     :stub "some"
                     :unparsed-stub "some"
                     :wordbreaks "")
                    'single))))

  ;; but only for a single completion
  (let ((bash-completion-nospace nil))
    (should
     (equal "somecmd"
            (bash-completion-fix
             "somecmd"
             (bash-completion--make
              :cword 0
              :stub "some"
              :unparsed-stub "some"
              :wordbreaks "")
             nil))))

  ;; subset of the prefix"
  (should (equal "Dexter"
		 (bash-completion-fix
                  "Dexter"
                  (bash-completion--make
                   :cword 1
                   :stub "Dexter'"
                   :unparsed-stub "Dexter'"
                   :wordbreaks "")
                  nil))))

(ert-deftest bash-completion-extract-candidates-test ()
  (let ((bash-completion-nospace nil))
    (should
     (equal
      '("hello\\ world" "hello ")
      (bash-completion-test-with-buffer
       "hello world\nhello \n\n"
       (bash-completion-extract-candidates
        (bash-completion--make :stub "hello"
                               :unparsed-stub "hello"
                               :wordbreaks ""
                               :cword 1)
        (current-buffer)))))
    (should
     (equal
      '("hello" "hellish" "hellow")
      (bash-completion-test-with-buffer
       "hello\nhellish\nhello\nhellow\n"
       (bash-completion-extract-candidates
        (bash-completion--make :stub "hell"
                               :unparsed-stub "hell"
                               :wordbreaks ""
                               :cword 1)
        (current-buffer)))))))

(ert-deftest bash-completion-extract-candidates-compopt-test ()
  (let ((bash-completion-nospace nil))
    (should
     (equal
      '("hello")
      (bash-completion-test-with-buffer
       "\e\ecompopt=-o nospace\e\ehello\n"
       (bash-completion-extract-candidates
        (bash-completion--make :stub "hell"
                               :unparsed-stub "hell"
                               :wordbreaks ""
                               :cword 1)
        (current-buffer)))))

    (should
     (equal
      '("hello ")
      (bash-completion-test-with-buffer
       "\e\ecompopt=+o nospace\e\ehello\n"
       (bash-completion-extract-candidates
        (bash-completion--make :stub "hell"
                               :unparsed-stub "hell"
                               :wordbreaks ""
                               :cword 1
                               :compgen-args '("-o" "nospace"))
        (current-buffer)))))))

(ert-deftest bash-completion-extract-candidates-ignore-compopt-test ()
  (let ((bash-completion-nospace t))
    (should
     (equal
      '("hello")
      (bash-completion-test-with-buffer
       "hello\n\e\ecompopt=+o nospace\e\e"
       (bash-completion-extract-candidates
        (bash-completion--make :stub "hell"
                               :unparsed-stub "hell"
                               :wordbreaks ""
                               :cword 1)
        (current-buffer)))))))

(ert-deftest bash-completion-nonsep-test ()
  (should (equal "^ \t\n\r;&|'\""
		 (bash-completion-nonsep nil "")))
  (should (equal "^ \t\n\r;&|'\":="
		 (bash-completion-nonsep nil ":=")))
  (should (equal "^ \t\n\r'"
		 (bash-completion-nonsep ?' "")))
  (should (equal "^ \t\n\r\""
		 (bash-completion-nonsep ?\" ""))))

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
the current directory in this environment is /tmp/test.

The body is run with a test buffer as current buffer. Fill it with the command-line
before calling `bash-completion-dynamic-complete-nocomint'.
"
  `(let ((default-directory "/tmp/test")
         (bash-completion-alist '())
         (bash-completion-use-separate-processes t)
         (wordbreaks "@><=;|&(:")
         (bash-completion-nospace nil))
     (let ((--process-buffer)
           (--test-buffer)
           (--send-results (list))
           (--captured-commands (list))
           (--directories (list)))
       (with-temp-buffer
         (setq --process-buffer (current-buffer))
         (with-temp-buffer
           (setq --test-buffer (current-buffer))
           (cl-letf (((symbol-function 'bash-completion--get-process) (lambda () 'process))
                     ((symbol-function 'process-put)
                      (lambda (process prop value)
                        (cond ((and (eq 'process process) (eq 'complete-p prop))
                               (setq bash-completion-alist value))
                              (t (error "unexpected: (process-put %s %s)" process prop)))))
                     ((symbol-function 'process-get)
                      (lambda (process prop)
                        (cond
                         ((and (eq 'process process) (eq 'complete-p prop))
                          bash-completion-alist)
                         ((and (eq 'process process) (eq 'wordbreaks prop))
                          wordbreaks)
                         ((and (eq 'process process) (eq 'completion-ignore-case prop))
                          completion-ignore-case)
                         (t (error "unexpected call")))))
                     ((symbol-function 'bash-completion-buffer) (lambda () --process-buffer))
                     ((symbol-function 'process-buffer) (lambda (p) --process-buffer))
                     ((symbol-function 'file-accessible-directory-p)
                      (lambda (d) (member d --directories)))
                     ((symbol-function 'bash-completion-send)
                      (lambda (commandline &optional process timeout debug-context)
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
   (should (equal "cd >/dev/null 2>&1 /tmp/test && compgen -o default -- he 2>/dev/null"
                  (pop --captured-commands)))))

(ert-deftest bash-completion-simple-dynamic-table-test ()
  (--with-fake-bash-completion-send
   (push "hell\nhello1\nhello2\n" --send-results)
   (insert "$ cat he")
   (pcase-let ((`(,stub-start ,stub-end ,completions)
                (bash-completion-dynamic-complete-nocomint
                 3 (point) 'dynamic-table)))
     (should (equal 7 stub-start))
     (should (equal 9 stub-end))
     (should (equal '("hell" "hello1" "hello2")
                    (funcall completions "he" nil t))))))

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
            '("\"hello\" ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-single-completion-single-quotes ()
  (--with-fake-bash-completion-send
   (push "hello\n" --send-results)
   (insert "$ cat 'he")
   (should (equal
            '("'hello' ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-completion-with-double-quotes ()
  (--with-fake-bash-completion-send
   (push "hell\nhello\n" --send-results)
   (insert "$ cat \"he")
   (should (equal
            '("\"hell\"" "\"hello\"")
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
             "cd >/dev/null 2>&1 /tmp/test && "
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
            '(7 11 ("'Documents/"))
            (bash-completion-dynamic-complete-nocomint 3 (point))))
   (insert "uments/")
   (push "Documents/Modes d'emplois\n" --send-results)
   (should (equal
            '("'Documents/Modes d'\\''emplois/")
            (nth 2(bash-completion-dynamic-complete-nocomint 3 (point)))))
   (insert "Modes d'\\''emplois/")
   (push "Documents/Modes d'emplois/KAR 1.pdf\nDocuments/Modes d'emplois/KAR 2.pdf\n"
         --send-results)
   (should (equal
            '("'Documents/Modes d'\\''emplois/KAR 1.pdf'"
              "'Documents/Modes d'\\''emplois/KAR 2.pdf'")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))))

(ert-deftest bash-completion-complete-command-with-dir ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/bin" --directories)
   (push "bin\nbind\n" --send-results)
   (insert "$ b")
   (should (equal
            '("bin/" "bind")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))
   (should (equal (concat "cd >/dev/null 2>&1 /tmp/test && "
                          "compgen -b -c -a -A function -- b 2>/dev/null")
                  (pop --captured-commands)))))

(ert-deftest bash-completion-complete-command-with-space ()
  (--with-fake-bash-completion-send
   (push "some command\n" --send-results)
   (insert "$ some\\ c")
   (should (equal
            '("some\\ command ")
            (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point)))))
   (should (equal (concat "cd >/dev/null 2>&1 /tmp/test && "
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
   (setq --send-results '("./binary\n./bind\n./bin\n"))
   (insert "$ export PATH=$PATH:./b")
   (should
    (equal '(21 24 ("./binary" "./bind" "./bin/"))
           (bash-completion-dynamic-complete-nocomint 3 (point))))))

(ert-deftest bash-completion-single-wordbreak-completion ()
  (--with-fake-bash-completion-send
   (push "/tmp/test/bin" --directories)
   (setq --send-results '("./world\n"))
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

(ert-deftest bash-completion-single-custom-completion-as-directory-with-option ()
  (--with-fake-bash-completion-send
   (setq bash-completion-alist '(("ls" "compgen" "args" "-o" "filenames")))
   ;; note that adding a / after a completion is not always the right thing
   ;; to do. See github issue #19.
   (push "/tmp/test/somedir" --directories)
   (push "somedir\n" --send-results)
   (insert "$ ls some")
   (let ((bash-completion-nospace nil))
     (should (equal
              '("somedir/")
              (nth 2 (bash-completion-dynamic-complete-nocomint 3 (point))))))))

(ert-deftest bash-completion--has-compgen-option ()
  (should (equal nil
                 (bash-completion--has-compgen-option
                  '("-F" "boo" "-o" "filenames" "-a" "-o" "default")
                  "nospace")))
  (should (equal t
                 (bash-completion--has-compgen-option
                  '("-F" "boo" "-o" "filenames" "-a" "-o" "default")
                  "filenames")))
  (should (equal t
                 (bash-completion--has-compgen-option
                  '("-F" "boo" "-o" "filenames" "-a" "-o" "default")
                  "default")))
  (should (equal nil (bash-completion--has-compgen-option
                      '("-o") "any")))
  (should (equal nil (bash-completion--has-compgen-option '() "any"))))

(ert-deftest bash-completion--parse-side-channel-data ()
  (bash-completion-test-with-buffer
   "test\ntest\e\ename=value\e\e\ntest"
   (should (equal
            "value"
            (bash-completion--parse-side-channel-data "name")))
   (should (equal "test\ntest\ntest" (buffer-string))))
  ;; leave other data alone
  (bash-completion-test-with-buffer
   "test\ntest\e\eothername=value\e\e\ntest"
   (should (null (bash-completion--parse-side-channel-data "name")))
   (should (equal "test\ntest\e\eothername=value\e\e\ntest" (buffer-string))))
  ;; name can contain chars special for regexps
  (bash-completion-test-with-buffer 
   "\e\ename*=value\e\etest"
   (should (equal "value" (bash-completion--parse-side-channel-data "name*")))
   (should (equal "test" (buffer-string)))))

;;; bash-completion_test.el ends here
