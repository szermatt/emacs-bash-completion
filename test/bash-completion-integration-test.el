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

(defmacro bash-completion_test-harness (bashrc &rest body)
  `(if (file-executable-p bash-completion-prog)
     (let ((test-env-dir (bash-completion_test-setup-env ,bashrc)))
       (let ((bash-completion-processes nil)
             (bash-completion-nospace nil)
             (bash-completion-start-files nil)
             (bash-completion-args
              (list "--noediting"
                    "--noprofile"
                    "--rcfile" (expand-file-name "bashrc" test-env-dir)))
             (kill-buffer-query-functions '())
             (minibuffer-message-timeout 0)
             (default-directory test-env-dir))
         ;; Give Emacs time to process any input or process state
         ;; change from bash-completion-reset.
         (while (accept-process-output nil 0.1))
         (unwind-protect
             (progn ,@body)
           (progn
             (bash-completion_test-teardown-env test-env-dir)
             (bash-completion-reset-all)))))))

(defmacro bash-completion_test-with-shell-harness (bashrc &rest body)
  `(bash-completion_test-harness
    ,bashrc
    (let ((shell-buffer))
      (unwind-protect
	  (progn
	    (setq shell-buffer (shell (generate-new-buffer-name
				       "*bash-completion_test-with-shell*")))
	    ;; accept process output until there's nothing left
	    (while (accept-process-output nil 0.6))
	    ;; do a completion and return the result
	    (with-current-buffer shell-buffer
              (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete))
                    (bash-major-version (process-get (bash-completion-require-process)
                                                     'bash-major-version)))
                (progn ,@body))))
        (progn ;; finally
          (when (and shell-buffer (buffer-live-p shell-buffer))
            (kill-process (get-buffer-process shell-buffer)))
          (when shell-buffer 
            (kill-buffer shell-buffer)))))))

(defun bash-completion_test-complete (complete-me)
  (goto-char (point-max))
  (comint-delete-input)
  (insert complete-me)
  (completion-at-point)
  (buffer-substring-no-properties
   (comint-line-beginning-position) (point)))

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
   (bash-completion-send "echo $EMACS_BASH_COMPLETE")
   (with-current-buffer (bash-completion-buffer)
     (should (equal "t\n" (buffer-string))))))

(ert-deftest bash-completion-integration-completion-test ()
  (bash-completion_test-with-shell-harness
   (concat ; .bashrc
    "function somefunction { echo ok; }\n"
    "function someotherfunction { echo ok; }\n"
    "function _dummy_complete {\n"
    "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then COMPREPLY=(dummy); fi\n"
    "}\n"
    "complete -F _dummy_complete -o filenames somefunction\n"
    "complete -F _dummy_complete -o default -o filenames someotherfunction\n")

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

(ert-deftest bash-completion-integration-bash-4-default-completion ()
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
   (when (>= bash-major-version 4)
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
   (when (>= bash-major-version 4)
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


;;; bash-completion-integration-test.el ends here
