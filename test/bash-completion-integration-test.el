;;; bash-completion-integration-test.el --- Integration tests for bash-completion.el

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
;; `bash-completion' that create a bash process.
;;

;;; History:
;;

;;; Code:
(require 'bash-completion)
(require 'dired)
(require 'ert)

(defvar bash-completion_test-notbash-prog (executable-find "dash")
  "Path to a command line executable that is not bash.

It must exit when given C-d as input.")

(defvar bash-completion_test-setup-completion "/etc/bash_completion")
(defconst bash-completion_test-start-mark "====START====")

(defmacro bash-completion_test-harness (bashrc use-separate-process &rest body)
  `(let ((test-env-dir (bash-completion_test-setup-env
                        (concat ,bashrc "\necho " bash-completion_test-start-mark "\n"))))
     (let ((bash-completion-processes nil)
           (bash-completion-nospace nil)
           (bash-completion-start-files nil)
           (bash-completion-use-separate-processes ,use-separate-process)
           (bash-completion-args
            (list "--noediting"
                  "--noprofile"
                  "--rcfile" (expand-file-name "bashrc" test-env-dir)))
           (bash-completion-short-command-timeout 3.0)
           (bash-completion-command-timeout 10.0)
           (bash-completion-initial-timeout 10.0)
           (bash-completion-process-timeout 5.0)
           (completion-ignore-case nil)
           (completion-in-region-function 'completion--in-region)
           (completion-cycle-threshold 20)
           (completion-styles '(basic partial-completion substring emacs22))
           (explicit-shell-file-name bash-completion-prog)
           (explicit-args-var (intern
                               (concat "explicit-"
                                       (file-name-nondirectory bash-completion-prog)
                                       "-args")))
           (inhibit-field-text-motion nil)
           (old-explicit-args)
           (shell-mode-hook nil)
           (comint-mode-hook nil)
           (kill-buffer-query-functions '())
           (minibuffer-message-timeout 0)
           (default-directory test-env-dir))
       ;; Set explicit-<executable name>-args for shell-mode.
       (when (boundp explicit-args-var)
         (setq old-explicit-args (symbol-value explicit-args-var)))
       (set explicit-args-var bash-completion-args)

       ;; Give Emacs time to process any input or process state
       ;; change from bash-completion-reset.
       (while (accept-process-output nil 0.1))
       (let ((realhome (getenv "HOME")))
         (unwind-protect
             (progn
               (setenv "HOME" test-env-dir)
               ,@body)
           (progn
             (setenv "HOME" realhome)
             (set explicit-args-var old-explicit-args)
             (bash-completion_test-teardown-env test-env-dir)
             (bash-completion-reset-all)))))))

(defmacro bash-completion_test-with-shell-harness (bashrc use-separate-process &rest body)
  `(bash-completion_test-harness
    ,bashrc
    ,use-separate-process
    (bash-completion_test-with-shell ,@body)))

(defmacro bash-completion_test-with-shell (&rest body)
  `(let ((shell-buffer))
     (unwind-protect
	 (progn
	   (setq shell-buffer (shell (generate-new-buffer-name
				      "*bash-completion_test-with-shell*")))
	   (with-current-buffer shell-buffer
             (bash-completion_test-wait-for-prompt)
             (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete))
                   (completion-at-point-functions '(comint-completion-at-point t)))
               (progn ,@body))))
       (when shell-buffer
         (when (and (buffer-live-p shell-buffer)
                    (get-buffer-process shell-buffer))
           (kill-process (get-buffer-process shell-buffer)))
         (kill-buffer shell-buffer)))))

(defun bash-completion_test-complete (complete-me)
  "Complete COMPLETE-ME and returns the resulting string."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (insert complete-me)
  (completion-at-point)
  (buffer-substring-no-properties
   (line-beginning-position) (point)))

(defun bash-completion_test-send-nowait (command)
  "Execute COMMAND in the current shell buffer.

This variant doesn't wait for the prompt to reappear."
  (delete-region (line-beginning-position) (line-end-position))
  (insert command)
  (comint-send-input))

(defun bash-completion_test-send (command &optional complete)
  "Execute COMMAND in a shell buffer.

Return a marker pointing to the beginning of the command."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (let ((command-start (point)))
    (prog1 (point-marker)
      (insert command)
      (when complete (completion-at-point))
      (comint-send-input)
      (bash-completion_test-wait-for-prompt command-start))))

(defun bash-completion_test-wait-for-prompt (&optional limit)
  (let ((process (get-buffer-process shell-buffer))
        (no-timeout t))
    (while (and no-timeout
                (not (re-search-backward "^.*$ " limit t)))
      (setq no-timeout (accept-process-output process 3.0 nil t)))
    no-timeout))

(defun bash-completion_test-buffer-string (&optional start end)
  (delete-trailing-whitespace (point-min) (point-max))
  (untabify (point-min) (point-max))
  (buffer-substring-no-properties
   (or start
       (save-excursion
         (goto-char (point-min))
         (1+ (search-forward bash-completion_test-start-mark end))))
   (or end (point-max))))

(defun bash-completion_test-candidates (complete-me &optional dynamic-table)
  "Complete COMPLETE-ME and returns the candidates.

The result is sorted to avoid hardcoding arbitrary order in the test."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (insert complete-me)
  (nth 2 (bash-completion-dynamic-complete-nocomint
          (line-beginning-position) (point) dynamic-table)))

(defun bash-completion_test-setup-env (bashrc)
  "Sets up a directory that contains a bashrc file other files
for testing completion."
  (let ((test-env-dir (make-temp-file
                       (expand-file-name "bash-completion_testenv"
                                         (or small-temporary-file-directory
                                             temporary-file-directory))
                       'mkdir)))
    (prog1
        test-env-dir
      (with-temp-file (expand-file-name "bashrc" test-env-dir)
        ;; Disable ZSH warning under MacOS Catalina
        (insert "export BASH_SILENCE_DEPRECATION_WARNING=1") 
        (insert (format "cd '%s'\n" test-env-dir))
        (insert bashrc)
        (insert "\n")
        (insert "HISTFILE=/dev/null\n")
        (insert "history -c\n"))
      (let ((default-directory test-env-dir))
        (with-temp-file "testfile1" (insert "test"))
        (with-temp-file "testfile2" (insert "test"))
        (with-temp-file "moretestfile" (insert "test"))
        (make-directory "some/directory" 'parents)
        (with-temp-file "some/testfile" (insert "test"))
        (make-directory "some/other/directory" 'parents)
        (with-temp-file "some/other/testfile1" (insert "test"))
        (with-temp-file "some/other/testfile2" (insert "test"))
        ))))

(defun bash-completion_test-teardown-env (test-env-dir)
  "Deletes everything `bash-completion_test-setup-env' set up."
  (when test-env-dir
    (delete-directory test-env-dir 'recursive)))

(defun bash-completion_test-equal-any-order (expected actual)
  "Compare a sorted list of string EXPECTED with ACTUAL.

Ignore the order of the strings in ACTUAL.

This is necessary, because the order of some results isn't stable
across Emacs version."
  (let ((actual (copy-tree actual)))
    (equal expected (sort actual 'string<))))

(ert-deftest bash-completion-integration-setenv-test ()
  (bash-completion_test-harness
   ""
   t ; use-separate-process
   (bash-completion-send "echo $EMACS_BASH_COMPLETE")
   (with-current-buffer (bash-completion-buffer)
     (should (equal "t\n" (buffer-string))))))

(ert-deftest bash-completion-integration-separate-processes-test ()
  (bash-completion_test-completion-test t))

(ert-deftest bash-completion-integration-single-process-test ()
  (bash-completion_test-completion-test nil))

(defun bash-completion_test-completion-test (use-separate-process)
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function somefunction { echo ok; }\n"
    "function someotherfunction { echo ok; }\n"
    "function _dummy_complete {\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then COMPREPLY=(dummy); fi\n"
    "}\n"
    "complete -F _dummy_complete -o filenames somefunction\n"
    "complete -F _dummy_complete -o default -o filenames someotherfunction\n")
   use-separate-process
   
   ;; complete bash builtin
   (should (equal "readonly "
                  (bash-completion_test-complete "reado")))
   ;; complete command
   (should (equal "somefunction "
                  (bash-completion_test-complete "somef")))
   ;; custom completion
   (should (equal "somefunction dummy "
                  (bash-completion_test-complete "somefunction du")))
   ;; function returns nothing, no -o default
   (should (equal "somefunction so"
                  (bash-completion_test-complete "somefunction so"))) ;
   ;; function returns nothing, -o default, so fallback to default 
   (should (equal "someotherfunction moretestfile "
                   (bash-completion_test-complete "someotherfunction moret")))
   ;; wordbreak completion
   (should (equal "export SOMEPATH=some/directory:some/testfile "
                  (bash-completion_test-complete
                   "export SOMEPATH=some/directory:some/te")))))

(ert-deftest bash-completion-integration-multiple-completions-test ()
  (bash-completion_test-multiple-completions-test ""))

(ert-deftest bash-completion-integration-multiple-completions-prog-test ()
  (bash-completion_test-multiple-completions-test
   (concat "source " bash-completion_test-setup-completion "\n")))

(defun bash-completion_test-multiple-completions-test (bashrc)
  (bash-completion_test-with-shell-harness
   bashrc
   nil ; use-separate-process
   (should (bash-completion_test-equal-any-order
            '("some/other/testfile1" "some/other/testfile2")
            (bash-completion_test-candidates "ls some/other/te")))
   (should (equal '("some/testfile ") (bash-completion_test-candidates "ls some/te")))
   (should (equal '("some/testfile ") (bash-completion_test-candidates "ls some/te")))
   (should (equal '() (bash-completion_test-candidates "ls some/do")))))

(ert-deftest bash-completion-integration-nocomint-test ()
  (bash-completion_test-harness
   "function somefunction { echo ok; }\n"
   nil ; use-separate-process=nil will be ignored
   (with-temp-buffer
     (let ((completion-at-point-functions
            (list (lambda () (bash-completion-dynamic-complete-nocomint
                              (line-beginning-position) (point) t)))))
       ;; complete bash builtin
       (should (equal "readonly "
                      (bash-completion_test-complete "reado")))
       ;; complete command
       (should (equal "somefunction "
                      (bash-completion_test-complete "somef")))))))

(ert-deftest bash-completion-integration-notbash-test ()
  (bash-completion_test-harness
   "function somefunction { echo ok; }\n"
   ; use-separate-process=nil will be ignored because the shell is not
   ; a bash shell.
   nil 
   (let ((explicit-shell-file-name "/bin/sh"))
     (bash-completion_test-with-shell
      ;; complete bash builtin
      (should (equal "readonly "
                     (bash-completion_test-complete "reado")))
      ;; complete command
      (should (equal "somefunction "
                     (bash-completion_test-complete "somef")))

      ;; make sure a separate process was used; in case /bin/sh is
      ;; actually bash, the test could otherwise work just fine.
      (should (not (null (cdr (assq nil bash-completion-processes)))))))))

(ert-deftest bash-completion-integration-notbash-anymore-test ()
  (skip-unless bash-completion_test-notbash-prog)
  (bash-completion_test-with-shell-harness
   "" ; bashrc
   nil ; use-separate-process
   ;; initially completion works
   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls testf")))
   ;; but then later on bash executes another command-line tool
   (bash-completion_test-send-nowait bash-completion_test-notbash-prog)
   (let ((bash-completion--debug-info nil))
     (should-error (bash-completion_test-candidates "ls testf"))
     (should (equal "short-timeout" (cdr (assq 'error bash-completion--debug-info)))))

   ;; this is recoverable, once the other command-line tool exits,
   ;; completion works again.
   (bash-completion_test-send-nowait "\004") ;; C-d

   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls testf")))))

(ert-deftest bash-completion-integration-bash-in-bash ()
  (skip-unless bash-completion_test-notbash-prog)
  (bash-completion_test-with-shell-harness
   "" ; bashrc
   nil ; use-separate-process
   ;; initially completion works
   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls testf")))

   ;; Start a bash subprocess. Note that Emacs normally adds
   ;; --noediting, which isn't set here; it should work nevertheless.
   (bash-completion_test-send bash-completion-prog)
   (bash-completion_test-send "HISTFILE=/dev/null") ;; avoid history file
   (bash-completion_test-send "function sometest { echo ok; }")
   (should (bash-completion_test-equal-any-order
            '("sometest ")
            (bash-completion_test-candidates "somet")))
   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls testf")))
   (bash-completion_test-send "exit")

   ;; back in the main bash process, completion results are different,
   ;; as sometest isn't defined.
   (should (bash-completion_test-equal-any-order
            '() (bash-completion_test-candidates "somet")))

   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls testf")))))

(ert-deftest bash-completion-integration-space ()
  ;; While completion generally works with Bash 4.0, 4.1, 4.2 and 4.3,
  ;; bash-completion.el appending / to directories doesn't work.
  (bash-completion_test-with-shell-harness
   ""
   t ; bash-completion-use-separate-processes
   (bash-completion_test-test-spaces)))

(ert-deftest bash-completion-integration-space-and-prog-completion ()
  (skip-unless (and bash-completion_test-setup-completion
                    (not (zerop (length bash-completion_test-setup-completion)))))
  ;; Recent version of bash completion define a completion for ls. This
  ;; test makes sure that it works.
  (bash-completion_test-with-shell-harness
   (concat "source " bash-completion_test-setup-completion "\n")
   t ; bash-completion-use-separate-processes
   (bash-completion_test-test-spaces)))
  
(defun bash-completion_test-test-spaces ()
   (make-directory "my dir1/my dir2" 'parents)
   (with-temp-file "my dir1/other" (insert "test"))

   (should (equal "ls my\\ dir1/" (bash-completion_test-complete "ls my")))
   (should (equal "ls my\\ dir1/my\\ dir2/" (bash-completion_test-complete "ls my\\ dir1/my")))
   (should (equal "ls my\\ dir1/other " (bash-completion_test-complete "ls my\\ dir1/o")))
   (should (equal "cp my\\ dir1/a my\\ dir1/" (bash-completion_test-complete "cp my\\ dir1/a my\\ dir")))

   (should (equal "ls \"my dir1/" (bash-completion_test-complete "ls \"my")))
   (should (equal "ls \"my dir1/my dir2/" (bash-completion_test-complete "ls \"my dir1/my")))
   (should (equal "ls \"my dir1/other\" " (bash-completion_test-complete "ls \"my dir1/o")))
   (should (equal "cp \"my dir1/a\" \"my dir1/" (bash-completion_test-complete "cp \"my dir1/a\" \"my dir")))

   (should (equal "ls 'my dir1/" (bash-completion_test-complete "ls 'my")))
   (should (equal "ls 'my dir1/my dir2/" (bash-completion_test-complete "ls 'my dir1/my")))
   (should (equal "ls 'my dir1/other' " (bash-completion_test-complete "ls 'my dir1/o")))
   (should (equal "cp 'my dir1/a' 'my dir1/" (bash-completion_test-complete "cp 'my dir1/a' 'my dir"))))

(ert-deftest bash-completion-integration-bash-4-default-completion ()
  (bash-completion_test-bash-4-default-completion t))

(ert-deftest bash-completion-integration-bash-4-default-completion-single-process ()
  (bash-completion_test-bash-4-default-completion nil))

(defun bash-completion_test-bash-4-default-completion (use-separate-process)
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _default {\n"
    "  if [[ ${COMP_WORDS[0]} == dosomething ]]; then\n"
    "    complete -F _dummy_complete ${COMP_WORDS[0]}\n"
    "    return 124\n"
    "  fi\n"
    "}\n"
    "function _dummy_complete {\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then COMPREPLY=(dummy); fi\n"
    "}\n"
    "complete -D -F _default\n")
   use-separate-process
   (should (equal "dosomething dummy "
                  (bash-completion_test-complete "dosomething du")))
   (should (equal "dosomethingelse du"
                  (bash-completion_test-complete "dosomethingelse du")))))

(ert-deftest bash-completion-integration-bash-4-compopt ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _sometimes_nospace {\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then\n"
    "    COMPREPLY=(dummy)\n"
    "  fi\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == dum ]]; then\n"
    "    COMPREPLY=(dummyo)\n"
    "    compopt -o nospace\n"
    "  fi\n"
    "}\n"
    "function _sometimes_not_nospace {\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then\n"
    "    COMPREPLY=(dummy)\n"
    "  fi\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == dum ]]; then \n"
    "    COMPREPLY=(dummyo)\n"
    "    compopt +o nospace\n"
    "  fi\n"
    "}\n"
    "complete -F _sometimes_nospace sometimes_nospace\n"
    "complete -F _sometimes_not_nospace -o nospace sometimes_not_nospace\n")
   t ; use-separate-process
   (should (equal
            "sometimes_nospace dummy "
            (bash-completion_test-complete "sometimes_nospace du")))
   (should (equal
            "sometimes_nospace dummyo"
            (bash-completion_test-complete "sometimes_nospace dum")))
   (should (equal
            "sometimes_not_nospace dummy"
            (bash-completion_test-complete "sometimes_not_nospace du")))
   (should (equal
            "sometimes_not_nospace dummyo "
            (bash-completion_test-complete "sometimes_not_nospace dum")))
   (let ((bash-completion-nospace t)) ;; never nospace
     (should (equal
              "sometimes_nospace dummy"
              (bash-completion_test-complete "sometimes_nospace du")))
     (should (equal
              "sometimes_not_nospace dummyo"
              (bash-completion_test-complete "sometimes_not_nospace dum"))))))

(ert-deftest bash-completion-integration-bash-4-substring-completion ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _myprog {\n"
    "  COMPREPLY=( \"ba${COMP_WORDS[$COMP_CWORD]}ta\" )\n"
    "}\n"
    "complete -F _myprog myprog\n")
   t ; bash-completion-use-separate-processes
   (should (equal
            "myprog blah batitita "
            (bash-completion_test-complete "myprog blah titi")))))

(ert-deftest bash-completion-integration-vioption-single-process-test ()
  (bash-completion_test--with-bash-option "set -o vi" nil))

(ert-deftest bash-completion-integration-vioption-multi-processes-test ()
  (bash-completion_test--with-bash-option "set -o vi" t))
  
(ert-deftest bash-completion-integration-emacsoption-single-process-test ()
  (bash-completion_test--with-bash-option "set -o emacs" nil))

(ert-deftest bash-completion-integration-emacsoption-multi-process-test ()
  (bash-completion_test--with-bash-option "set -o emacs" t))

(defun bash-completion_test--with-bash-option (turn-on-option use-separate-process)
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    turn-on-option "\n"
    "function _dummy { COMPREPLY=(Yooo); }\n"
    "function dummy { echo $1; }\n"
    "complete -F _dummy dummy\n")
   use-separate-process
   (should (equal "dummy 1 Yooo "
                  (bash-completion_test-complete "dummy 1 Y")))))

(ert-deftest bash-completion-integration-tracing ()
  (bash-completion_test-with-shell-harness
   "set -x\n" ;; .bashrc
   nil ;; use-separate-process
   (should (bash-completion_test-equal-any-order
            '("testfile1" "testfile2")
            (bash-completion_test-candidates "ls te")))))

(ert-deftest bash-completion-integration-refresh-test ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _dummy { COMPREPLY=(Yooo); }\n"
    "function dummy { echo $1; }\n"
    "complete -F _dummy dummy\n")
   nil ; use-separate-process
   (should (equal "dummy 1 Yooo "
                  (bash-completion_test-complete "dummy 1 Y")))
   (bash-completion_test-send "function _dummy2 { COMPREPLY=(Yaaa); }")
   (bash-completion_test-send "complete -F _dummy2 dummy")
   (bash-completion-refresh)
   (should (equal "dummy 1 Yaaa "
                  (bash-completion_test-complete "dummy 1 Y")))))

(ert-deftest bash-completion-integration-default ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _ook {\n"
    "  COMPREPLY=(ook)\n"
    "}\n"
    "function _bar {\n"
    "  COMPREPLY=(bar)\n"
    "}\n"
    "function _baa {\n"
    "  COMPREPLY=(baa)\n"
    "}\n"
    "complete -F _ook -D\n"
    "complete -F _bar foo\n"
    "complete -F _baa baa\n")
   t ; use-separate-process
   ;; using default (_ook)
   (should (equal
            "faa ook "
            (bash-completion_test-complete "faa a")))
   (should (equal
            "fee ook "
            (bash-completion_test-complete "fee a")))
   ;; exceptions, to make sure non-default still works
   (should (equal
            "foo bar "
            (bash-completion_test-complete "foo a")))
   (should (equal
            "baa baa "
            (bash-completion_test-complete "baa a")))))

(ert-deftest bash-completion-integration-case-insensitive-test ()
  (bash-completion_test-harness
   (concat ; .bashrc
    "INPUTRC=test-inputrc\n")
   nil ; use-separate-process
   (with-temp-file "test-inputrc"
     (insert "set completion-ignore-case on\n"))
   (with-temp-file "Uppercase" (insert "test"))
   (with-temp-file "Another Uppercase" (insert "test"))
   (with-temp-file "libs" (insert "test"))
   (with-temp-file "library" (insert "test"))
   (bash-completion_test-with-shell
    ;; Case insensitive completion is done by compgen which, under
    ;; bash 4, respects the case sensitivity settings set in
    ;; .inputrc.
    (should (equal "ls testfile1" (bash-completion_test-complete "ls testf")))
    (should (equal "ls testfile1" (bash-completion_test-complete "ls TestF")))
    (should (equal "ls Uppercase " (bash-completion_test-complete "ls Up")))
    (should (equal "ls Uppercase " (bash-completion_test-complete "ls up")))
    
    (should (equal "ls libs" (bash-completion_test-complete "ls li")))
    (should (equal "ls libs" (bash-completion_test-complete "ls Li")))

    (should (equal "ls Another\\ Uppercase " (bash-completion_test-complete "ls Ano")))
    (should (equal "ls Another\\ Uppercase " (bash-completion_test-complete "ls ano")))
    (should (equal "ls \"Another Uppercase\" " (bash-completion_test-complete "ls \"Ano")))
    (should (equal "ls \"Another Uppercase\" " (bash-completion_test-complete "ls \"ano")))
    (should (equal "ls 'Another Uppercase' " (bash-completion_test-complete "ls 'Ano")))
    (should (equal "ls 'Another Uppercase' " (bash-completion_test-complete "ls 'ano")))

    ;; When doing case-insensitive search, bash-completion.el cannot
    ;; keep the exact same quotes, so it just puts the quote, if
    ;; any, at the beginning, just after the tilde part.
    (should (equal "ls \"Another Uppercase\" " (bash-completion_test-complete "ls ano\"t")))
    (should (equal "ls 'Another Uppercase' " (bash-completion_test-complete "ls ano't")))
    (should (equal "ls ~/\"Another Uppercase\" " (bash-completion_test-complete "ls ~/ano\"t"))))))

(ert-deftest bash-completion-integration-case-sensitive-test ()
  (bash-completion_test-harness
   (concat ; .bashrc
    "INPUTRC=test-inputrc\n")
   nil ; use-separate-process
   (with-temp-file "test-inputrc"
     (insert "set completion-ignore-case off\n"))
   (with-temp-file "Uppercase" (insert "test"))
   (bash-completion_test-with-shell
    (should (equal "ls moretestfile " (bash-completion_test-complete "ls moret")))
    (should (equal "ls Te" (bash-completion_test-complete "ls Te")))
    (should (equal "ls Uppercase " (bash-completion_test-complete "ls Up")))
    (should (equal "ls up" (bash-completion_test-complete "ls up")))
    (should (not completion-ignore-case)))))

(ert-deftest bash-completion-integration-tilde-test ()
  (bash-completion_test-with-shell-harness
   ""
   nil ; use-separate-process
   (should (equal "ls moretestfile " (bash-completion_test-complete "ls morete")))
   (should (equal "ls ~/moretestfile " (bash-completion_test-complete "ls ~/morete")))
   (should (equal "ls \"~/moretestfile\" " (bash-completion_test-complete "ls \"~/morete")))
   (should (equal "ls '~/moretestfile' " (bash-completion_test-complete "ls '~/morete")))))

(ert-deftest bash-completion-integration-prompt-command ()
  "Tests PROMPT_COMMAND storage and recovery in single-process mode."
  (bash-completion_test-with-shell-harness
   "prompt_count=0
function _prompt {
  PS1=\"[$prompt_count]:$? $ \"
  prompt_count=$(( $prompt_count + 1 ))
}
PROMPT_COMMAND=_prompt
"
   nil ; use-separate-process
   (bash-completion_test-send "ls -1 morete" 'complete)
   (bash-completion_test-send "tru" 'complete)
   (bash-completion_test-send "fals" 'complete)
   ;; PROMPT_COMMAND is reset early on to avoid ever running it when
   ;; doing completion.
   (should (equal
            (bash-completion_test-buffer-string)
            "[0]:0 $ ls -1 moretestfile
moretestfile
[1]:0 $ true
[2]:0 $ false
[3]:1 $ "))))

(ert-deftest bash-completion-integration-ps1 ()
  "Tests PS1 storage and recovery in single-process mode."
  (bash-completion_test-with-shell-harness
   "PS1='$? $ '"
   nil ; use-separate-process
   (bash-completion_test-send "ls -1 morete" 'complete)
   (bash-completion_test-send "tru" 'complete)
   (bash-completion_test-send "fals" 'complete)
   (should (equal
            (bash-completion_test-buffer-string)
            "0 $ ls -1 moretestfile
moretestfile
0 $ true
0 $ false
1 $ "))))

(ert-deftest bash-completion-integration-prompt-history ()
  "Tests that history is not polluted by completion."
  (bash-completion_test-with-shell-harness
   "PS1='$ '"
   nil ; use-separate-process
   (bash-completion_test-send "ls -1 morete" 'complete)
   (bash-completion_test-send "tru" 'complete)
   (bash-completion_test-send "fals" 'complete)
   (let ((history-start (bash-completion_test-send "history")))
     (untabify (point-min) (point-max))
     (delete-trailing-whitespace (point-min) (point-max))
     (should (equal
              (bash-completion_test-buffer-string history-start)
              "history
    1  ls -1 moretestfile
    2  true
    3  false
    4  history
$ ")))))

(ert-deftest bash-completion-integration-bad-directory-tracking ()
  "When using single-process, bad directory tracking shouldn't be a problem."
  (bash-completion_test-with-shell-harness
   ""  ; .bashrc
   nil ; use-separate-process
   (let ((default-directory "/does-not-exist/"))
     (should (equal "ls moretestfile " (bash-completion_test-complete "ls moret"))))))

(ert-deftest bash-completion-integration-caching ()
  "Make sure caching works and that completion is only executed once."
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "PS1='$ '\n"
    "dummycount=0;\n"
    "function _dummy {\n"
    "  dummycount=$(( $dummycount + 1 ))\n"
    "  COMPREPLY=(libra library librarian)\n"
    "}\n"
    "function dummy { echo count: ${dummycount}.; }\n"
    "complete -F _dummy dummy\n")
   nil ; use-separate-process
   (bash-completion_test-send "dummy libr" 'complete)
   (should (string-match
            (regexp-quote "$ dummy libra\ncount: 1.")
            (bash-completion_test-buffer-string)))))

(ert-deftest bash-completion_test-dynamic-table ()
  (bash-completion_test-with-shell-harness
   ""
   nil ; use-separate-process
   (let ((default-directory test-env-dir))
     (with-temp-file "some/file" (insert "test"))
     (with-temp-file "some/filled" (insert "test")))
   (let ((compfunc-some (bash-completion_test-candidates "ls some/f" 'dynamic-table))
         (compfunc-one (bash-completion_test-candidates "ls some/fill" 'dynamic-table)))

     ;; Behavior of the completion function should match the one
     ;; described in:
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html
     
     ;; all-completion
     (should (bash-completion_test-equal-any-order
              '("some/file" "some/filled") (funcall compfunc-some "some/" nil t)))
     (should (equal '("some/filled") (funcall compfunc-some "some/fill" nil t)))
     (should (equal nil (funcall compfunc-some "other" nil t)))
     (should (equal '("some/filled ") (funcall compfunc-one "some/fill" nil t)))

     ;; all-completion with predicate
     (should (equal '("some/file")
                    (funcall compfunc-some "some/"
                             (lambda (c) (string= c "some/file")) t)))
     (should (equal nil
                    (funcall compfunc-some "some/" (lambda (c) nil) t)))
     (should (bash-completion_test-equal-any-order
              '("some/file" "some/filled")
              (funcall compfunc-some "some/" (lambda (c) t) t)))

     ;; try-completion
     (should (equal "some/filled " (funcall compfunc-one "some/fill" nil nil)))
     (should (equal "some/fil" (funcall compfunc-some "some/" nil nil)))
     (should (equal t (funcall compfunc-some "some/file" nil nil)))
     (should (equal t (funcall compfunc-one "some/filled " nil nil)))

     ;; try-completion with predicate
     (should (equal "some/file"
                    (funcall compfunc-some "some/"
                             (lambda (c) (string= c "some/file")) nil)))
     
     ;; test-completion
     (should (equal nil (funcall compfunc-some "some/" nil 'lambda)))
     (should (equal t (funcall compfunc-some "some/file" nil 'lambda)))
     (should (equal t (funcall compfunc-some "some/filled" nil 'lambda)))
     (should (equal nil (funcall compfunc-one "some/fill" nil 'lambda)))
     (should (equal t (funcall compfunc-one "some/filled " nil 'lambda)))

     ;; test-completion with predicate
     (should (equal nil (funcall compfunc-some "some/" nil 'lambda)))
     (should (equal nil (funcall compfunc-some "some/file/"
                                 (lambda (c) (string= c "some/filled")) 'lambda)))
     (should (equal t (funcall compfunc-some "some/filled"
                               (lambda (c) (string= c "some/filled")) 'lambda)))

     ;; completion-boundaries (not supported)
     (should (equal nil (funcall compfunc-some "some/" nil '(boundaries . "/"))))
     (should (equal nil (funcall compfunc-one "some/fill" nil '(boundaries . "/"))))

     ;; metadata (not supported)
     (should (equal nil (funcall compfunc-some "some/" nil 'metadata)))
     (should (equal nil (funcall compfunc-one "some/fill" nil 'metadata)))

     ;; some unknown value
     (should (equal nil (funcall compfunc-some "some/" nil 'unknown)))
     (should (equal nil (funcall compfunc-one "some/fill" nil 'unknown))))))

(ert-deftest bash-completion_test-dynamic-table-no-results ()
  (bash-completion_test-with-shell-harness
   ""
   nil ; use-separate-process
   (let ((compfunc-none (bash-completion_test-candidates "ls none/" 'dynamic-table)))

     ;; all-completion
     (should (equal nil (funcall compfunc-none "none/" nil t)))

     ;; try-completion
     (should (equal nil (funcall compfunc-none "none/" nil nil)))

     ;; test-completion
     (should (equal nil (funcall compfunc-none "none/" nil 'lambda))))))

(ert-deftest bash-completion_test-dynamic-table-nonprefix ()
  (bash-completion_test-with-shell-harness
   (concat "function _myprog {\n"
           "  COMPREPLY=( \"ba${COMP_WORDS[$COMP_CWORD]}dai\" \n"
           "              \"ba${COMP_WORDS[$COMP_CWORD]}tai\"  )\n"
           "}\n"
           "complete -F _myprog myprog\n")
   nil ; use-separate-process
   (let ((compfunc-nonprefix (bash-completion_test-candidates
                              "myprog bee" 'dynamic-table)))
     
     ;; all-completion
     ;;
     ;; all-completion doesn't strictly follow spec when the
     ;; completion doesn't start with the word completed. The goal is
     ;; for the completion to be displayed unless the user edited the
     ;; text string, so the first time all-completion is called with
     ;; the current word, all completions are returned, even the one
     ;; that don't match the string as prefix.
     (should (bash-completion_test-equal-any-order
              '("babeedai" "babeetai") (funcall compfunc-nonprefix "bee" nil t)))
     (should (bash-completion_test-equal-any-order
              '("babeedai" "babeetai") (funcall compfunc-nonprefix "babee" nil t)))
     (should (equal '("babeetai") (funcall compfunc-nonprefix "babeet" nil t)))

     ;; all-completion with predicate
     (should (equal '("babeetai") (funcall compfunc-nonprefix "bee"
                                           (lambda (c) (equal "babeetai" c))
                                           t)))

     ;; try-completion
     ;;
     ;; try-completion behaves in a way that's consistent with
     ;; all-completion: as long as it's passed the same string that's
     ;; completed, it won't filter the results.
     (should (equal "babee" (funcall compfunc-nonprefix "bee" nil nil)))
     (should (equal "babee" (funcall compfunc-nonprefix "bab" nil nil)))
     (should (equal "babeetai" (funcall compfunc-nonprefix "babeet" nil nil)))
     (should (equal t (funcall compfunc-nonprefix "babeetai" nil nil)))

     ;; try-completion with predicate
     (should (equal "babeetai" (funcall compfunc-nonprefix "bee"
                                        (lambda (c) (equal "babeetai" c)) nil)))
     (should (equal "babeetai" (funcall compfunc-nonprefix "bab"
                                        (lambda (c) (equal "babeetai" c)) nil)))
     
     ;; test-completion
     ;;
     ;; test-completion works according to spec.
     (should (equal nil (funcall compfunc-nonprefix "bee" nil 'lambda)))
     (should (equal nil (funcall compfunc-nonprefix "bab" nil 'lambda)))
     (should (equal t (funcall compfunc-nonprefix "babeetai" nil 'lambda)))
     
     ;; test-completion with predicate
     (should (equal t (funcall compfunc-nonprefix "babeetai"
                               (lambda (c) (equal "babeetai" c)) 'lambda)))
     (should (equal nil (funcall compfunc-nonprefix "babeetai"
                                 (lambda (c) (equal "babeedai" c)) 'lambda))))))

(ert-deftest bash-completion_test-issue-74 ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "_mycmd() {\n"
    "  readarray -t opts < <(printf \"foo\nbar\")\n"
    "  COMPREPLY=($(compgen -W \"${opts[*]}\" -- \"${COMP_WORDS[$COMP_CWORD]}\"))\n"
    "}\n"
    "complete -F _mycmd mycmd\n")
   nil ;; use-separate-process

   (should (equal "mycmd bar "
                  (bash-completion_test-complete "mycmd b")))))

(ert-deftest bash-completion-integration-filenames-option ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _dummy {\n"
    "}\n"
    "complete -F _dummy -o plusdir dummy\n")
   nil
   (should (equal
            "dummy some/other/"
            (bash-completion_test-complete "dummy some/ot")))))

(ert-deftest bash-completion-integration-plusdir-option ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _dummy {\n"
    "}\n"
    "complete -F _dummy -o filenames dummy\n")
   nil
   (should (equal
            "dummy moretestfile "
            (bash-completion_test-complete "dummy moret")))))

(ert-deftest bash-completion-integration-new-compopt-options ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function _dummy {\n"
    "    compopt -o default -o bashdefault\n"
    "}\n"
    "complete -F _dummy dummy\n")
   nil
   (should (equal
            "dummy moretestfile "
            (bash-completion_test-complete "dummy moret")))))

(ert-deftest bash-completion-integration-recover-status-ps1 ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function dummy { echo --$PS1--; }\n"
    "function _dummy {\n"
    "    COMPREPLY=( dummy )\n"
    "}\n"
    "complete -F _dummy dummy\n"
    "PS1='$ '")
   nil
   ;; The first time initializes completion, the second time executes
   ;; an already initialized completion. The two cases behave very
   ;; differently, so we test both.
   (dotimes (i 2)
     (should (equal
              "dummy dummy "
              (bash-completion_test-complete "dummy dum")))
     (let ((start (line-beginning-position)))
       (comint-send-input)
       (bash-completion_test-wait-for-prompt start)))

   ;; The PS1 printed by the dummy function should be the one set in
   ;; the init section, and not the one set by bash completion.
   (should (equal (bash-completion_test-buffer-string)
                  "$ dummy dummy\n--$ --\n$ dummy dummy\n--$ --\n$ "))))

(ert-deftest bash-completion-integration-recover-status-code ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function failwith { return $1; }\n"
    "function dummy { echo $?; }\n"
    "function _dummy {\n"
    "    COMPREPLY=( dummy )\n"
    "}\n"
    "complete -F _dummy dummy\n"
    "PS1='\$ '")
   nil
   ;; The first time initializes completion, the second time executes
   ;; an already initialized completion. The two cases behave very
   ;; differently, so we test both.
   (dotimes (i 2)
     (bash-completion_test-send (format "failwith %s" (+ 100 i)))
     (should (equal
              "dummy dummy "
              (bash-completion_test-complete "dummy dum")))
     (let ((start (line-beginning-position)))
       (comint-send-input)
       (bash-completion_test-wait-for-prompt start)))
   ;; The status code printed by the dummy function should be the one
   ;; from testfail, so 123, and not the one from the completion
   ;; command executed to do completion for the dummy function.
   (should (equal (bash-completion_test-buffer-string)
                  (concat "$ failwith 100\n"
                          "$ dummy dummy\n"
                          "100\n"
                          "$ failwith 101\n"
                          "$ dummy dummy\n"
                          "101\n"
                          "$ ")))))

(ert-deftest bash-completion-keep-existing-trap ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "calls=0\n"
    "function _calltrap {\n"
    " calls=$((calls+1))\n"
    "}\n"
    "trap _calltrap DEBUG\n"
    "PS1='\$ '")
   nil
   (bash-completion_test-send "n=$calls")
   (bash-completion_test-send "tru" 'complete)
   (bash-completion_test-send "fals" 'complete)
   (bash-completion_test-send "[[ $calls -gt $n ]] && echo ok")
   (bash-completion_test-send "trap -p DEBUG")
   (should (equal (bash-completion_test-buffer-string)
                  (concat
                   "$ n=$calls\n"
                   "$ true\n"
                   "$ false\n"
                   "$ [[ $calls -gt $n ]] && echo ok\n"
                   "ok\n"
                   "$ trap -p DEBUG\n"
                   "trap -- '_calltrap; __ebctrap' DEBUG\n"
                   "$ ")))))

(ert-deftest bash-completion-prompt-after-syntax-error-issue-79 ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "PS1='\$ '")
   nil
   (goto-char (point-max))
   (let ((start (point)))
     (insert "function foobar")
     (completion-at-point)
     (insert " {}")
     (comint-send-input)
     (bash-completion_test-wait-for-prompt start))))

(ert-deftest bash-completion-command-in-prompt-issue-80 ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "PS1='`echo boo`$ '")
   nil
   (goto-char (point-max))
   (let ((start (point)))
     (should
      (equal
       "function "
       (bash-completion_test-complete "functi")))
     (bash-completion_test-wait-for-prompt start))
   (should (equal (bash-completion_test-buffer-string)
                  (concat
                   "boo$ function")))))

;;; bash-completion-integration-test.el ends here
