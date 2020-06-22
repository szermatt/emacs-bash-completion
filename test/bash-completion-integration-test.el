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

(defvar bash-completion_test-setup-completion "/etc/bash_completion")

(defmacro bash-completion_test-harness (bashrc use-separate-process &rest body)
  `(let ((test-env-dir (bash-completion_test-setup-env ,bashrc)))
     (let ((bash-completion-processes nil)
           (bash-completion-nospace nil)
           (bash-completion-start-files nil)
           (bash-completion-use-separate-processes ,use-separate-process)
           (bash-completion-args
            (list "--noediting"
                  "--noprofile"
                  "--rcfile" (expand-file-name "bashrc" test-env-dir)))
           (completion-ignore-case nil)
           (completion-in-region-function 'completion--in-region)
           (completion-cycle-threshold 20)
           (completion-styles '(basic partial-completion substring emacs22))
           (explicit-shell-file-name bash-completion-prog)
           (explicit-args-var (intern
                               (concat "explicit-"
                                       (file-name-nondirectory bash-completion-prog)
                                       "-args")))
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
             (bash-completion--wait-for-regexp
              (get-buffer-process shell-buffer) comint-prompt-regexp 3.0)
             (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete))
                   (completion-at-point-functions '(comint-completion-at-point t)))
               (progn ,@body))))
       (when shell-buffer
         (when (and (buffer-live-p shell-buffer)
                    (get-buffer-process shell-buffer))
           (kill-process (get-buffer-process shell-buffer)))
         (kill-buffer shell-buffer)))))

(defun bash-completion_test-bash-major-version ()
  "Return the major version of the bash process."
  (process-get (bash-completion--get-process) 'bash-major-version))

(defun bash-completion_test-complete (complete-me)
  "Complete COMPLETE-ME and returns the resulting string."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (insert complete-me)
  (completion-at-point)
  (buffer-substring-no-properties
   (line-beginning-position) (point)))

(defun bash-completion_test-send (command)
  "Execute COMMAND in a shell buffer."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (insert command)
  (comint-send-input)
  (bash-completion--wait-for-regexp
   (get-buffer-process (current-buffer)) comint-prompt-regexp 3.0))

(defun bash-completion_test-candidates (complete-me)
  "Complete COMPLETE-ME and returns the candidates."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position))
  (insert complete-me)
  (nth 2 (bash-completion-dynamic-complete-nocomint)))

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
        (insert (format "cd '%s'\n" test-env-dir))
        (insert bashrc))
      (let ((default-directory test-env-dir))
        (make-directory "some/directory" 'parents)
        (make-directory "some/other/directory" 'parents)))))

(defun bash-completion_test-teardown-env (test-env-dir)
  "Deletes everything `bash-completion_test-setup-env' set up."
  (when test-env-dir
    (if (>= emacs-major-version 24)
        (delete-directory test-env-dir 'recursive)
      (dired-delete-file test-env-dir 'always))))

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
   (should (equal "someotherfunction some/"
                   (bash-completion_test-complete "someotherfunction so")))
   ;; wordbreak completion
   (should (equal "export SOMEPATH=some/directory:some/other/"
                  (bash-completion_test-complete
                   "export SOMEPATH=some/directory:some/oth")))))

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

(ert-deftest bash-completion-integration-space ()
  (bash-completion_test-with-shell-harness
   ""
   t ; bash-completion-use-separate-processes
   (bash-completion_test-test-spaces)))

(ert-deftest bash-completion-integration-space-and-prog-completion ()
  ;; Recent version of bash completion define a completion for ls. This
  ;; test makes sure that it works.
  (when (and bash-completion_test-setup-completion
             (not (zerop (length bash-completion_test-setup-completion))))
    (bash-completion_test-with-shell-harness
     (concat "source " bash-completion_test-setup-completion "\n")
     t ; bash-completion-use-separate-processes
     (bash-completion_test-test-spaces))))
  
(defun bash-completion_test-test-spaces ()
   (make-directory "my dir1/my dir2" 'parents)
   (with-temp-buffer (write-file "my dir1/other"))

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
   (when (>= (bash-completion_test-bash-major-version) 4)
     (should (equal "dosomething dummy "
                    (bash-completion_test-complete "dosomething du")))
     (should (equal "dosomethingelse du"
                      (bash-completion_test-complete "dosomethingelse du"))))))

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
   (when (>= (bash-completion_test-bash-major-version) 4)
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
                (bash-completion_test-complete "sometimes_not_nospace dum")))))))

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

(ert-deftest bash-completion-integration-case-insensitive-test ()
  (bash-completion_test-harness
   (concat ; .bashrc
    "INPUTRC=test-inputrc\n")
   nil ; use-separate-process
   (with-temp-file "test-inputrc"
     (insert "set completion-ignore-case on\n"))
   (make-directory "Uppercase")
   (make-directory "Another Uppercase")
   (make-directory "libs")
   (make-directory "Library")
   (bash-completion_test-with-shell
    (when (>= (bash-completion_test-bash-major-version) 4)
      ;; Case insensitive completion is done by compgen which, under
      ;; bash 4, respects the case sensitivity settings set in
      ;; .inputrc.
      (should (equal "ls some/" (bash-completion_test-complete "ls so")))
      (should (equal "ls some/" (bash-completion_test-complete "ls So")))
      (should (equal "ls Uppercase/" (bash-completion_test-complete "ls Up")))
      (should (equal "ls Uppercase/" (bash-completion_test-complete "ls up")))
      
      (should (equal "ls libs/" (bash-completion_test-complete "ls li")))
      (should (equal "ls libs/" (bash-completion_test-complete "ls Li")))

      (should (equal "ls Another\\ Uppercase/" (bash-completion_test-complete "ls Ano")))
      (should (equal "ls Another\\ Uppercase/" (bash-completion_test-complete "ls ano")))
      (should (equal "ls \"Another Uppercase/" (bash-completion_test-complete "ls \"Ano")))
      (should (equal "ls \"Another Uppercase/" (bash-completion_test-complete "ls \"ano")))
      (should (equal "ls 'Another Uppercase/" (bash-completion_test-complete "ls 'Ano")))
      (should (equal "ls 'Another Uppercase/" (bash-completion_test-complete "ls 'ano")))

      ;; When doing case-insensitive search, bash-completion.el cannot
      ;; keep the exact same quotes, so it just puts the quote, if
      ;; any, at the beginning, just after the tilde part.
      (should (equal "ls \"Another Uppercase/" (bash-completion_test-complete "ls ano\"t")))
      (should (equal "ls 'Another Uppercase/" (bash-completion_test-complete "ls ano't")))
      (should (equal "ls ~/\"Another Uppercase/" (bash-completion_test-complete "ls ~/ano\"t")))))))      

(ert-deftest bash-completion-integration-case-sensitive-test ()
  (bash-completion_test-harness
   (concat ; .bashrc
    "INPUTRC=test-inputrc\n")
   nil ; use-separate-process
   (with-temp-file "test-inputrc"
     (insert "set completion-ignore-case off\n"))
   (make-directory "Uppercase")
   (bash-completion_test-with-shell
    (should (equal "ls some/" (bash-completion_test-complete "ls so")))
    (should (equal "ls So" (bash-completion_test-complete "ls So")))
    (should (equal "ls Uppercase/" (bash-completion_test-complete "ls Up")))
    (should (equal "ls up" (bash-completion_test-complete "ls up")))
    (should (not completion-ignore-case)))))

(ert-deftest bash-completion-integration-tilde-test ()
  (bash-completion_test-with-shell-harness
   ""
   nil ; use-separate-process
   (should (equal "ls some/" (bash-completion_test-complete "ls so")))
   (should (equal "ls ~/some/" (bash-completion_test-complete "ls ~/so")))
   (should (equal "ls \"~/some/" (bash-completion_test-complete "ls \"~/so")))
   (should (equal "ls '~/some/" (bash-completion_test-complete "ls '~/so")))))

;;; bash-completion-integration-test.el ends here
