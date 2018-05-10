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

(defmacro bash-completion_test-harness (&rest body)
  `(if (file-executable-p bash-completion-prog)
     (let ((test-env-dir (bash-completion_test-setup-env)))
       (let ((bash-completion-processes nil)
             (bash-completion-nospace 'as-configured)
             (bash-completion-enable-caching nil)
             (bash-completion-start-files nil)
             (bash-completion-args
              (list "--noediting"
                    "--noprofile"
                    "--rcfile" (expand-file-name "bashrc" test-env-dir)))
             (kill-buffer-query-functions '())
             (default-directory test-env-dir))
         ;; Give Emacs time to process any input or process state
         ;; change from bash-completion-reset.
         (while (accept-process-output nil 0.1))
         (unwind-protect
             (progn ,@body)
           (progn
             (bash-completion_test-teardown-env test-env-dir)
             (bash-completion-reset-all)))))))

(defmacro bash-completion_test-with-shell-harness (&rest body)
  `(bash-completion_test-harness
    (let ((shell-buffer))
      (unwind-protect
	  (progn
	    (setq shell-buffer (shell (generate-new-buffer-name
				       "*bash-completion_test-with-shell*")))
	    ;; accept process output until there's nothing left
	    (while (accept-process-output nil 0.6))
	    ;; do a completion and return the result
	    (with-current-buffer shell-buffer
              (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete)))
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

(defun bash-completion_test-setup-env ()
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
        (insert "function somefunction { echo ok; }\n")
        (insert "function someotherfunction { echo ok; }\n")
        (insert "function _dummy_complete {\n")
        (insert "  if [[ ${COMP_WORDS[COMP_CWORD]} == du ]]; then COMPREPLY=(dummy); fi\n")
        (insert "}\n")
        (insert "complete -F _dummy_complete -o filenames somefunction\n")
        (insert "complete -F _dummy_complete -o default -o filenames someotherfunction\n"))
      (let ((default-directory test-env-dir))
        (make-directory "some/directory" 'parents)
        (make-directory "some/other/directory" 'parents)))))

(defun bash-completion_test-teardown-env (test-env-dir)
  "Deletes everything `bash-completion_test-setup-env' set up."
  (when test-env-dir
    (if (>= emacs-major-version 24)
        (delete-directory test-env-dir 'recursive)
      (dired-delete-file test-env-dir 'always))))

(ert-deftest bash-completion-integration-test ()
  (bash-completion_test-harness
   (should-not (bash-completion-is-running))
   (should (buffer-live-p (bash-completion-buffer)))
   (should (bash-completion-is-running))
   (should-not (null (member
                      "help "
                      (let ((bash-completion-nospace nil))
                        (bash-completion-comm
                         (bash-completion--make
                          :line "hel"
                          :point 4
                          :words '("hel")
                          :cword 0
                          :unparsed-stub "hel"))))))
   (bash-completion-reset)
   (should-not (bash-completion-is-running))))

(ert-deftest bash-completion-integration-setenv-test ()
  (bash-completion_test-harness
   (bash-completion-send "echo $EMACS_BASH_COMPLETE")
   (with-current-buffer (bash-completion-buffer)
     (should (equal "t\n" (buffer-string))))))

(ert-deftest bash-completion-integration-completion-test ()
  (bash-completion_test-with-shell-harness
   (bash-completion-integration-test-complete)))

(ert-deftest bash-completion-integration-completion-test-with-caching ()
  (bash-completion_test-with-shell-harness
   (setq bash-completion-enable-caching t)
   (bash-completion-integration-test-complete)))

(defun bash-completion-integration-test-complete ()
   ;; complete command
   (should (equal "somefunction "
                  (bash-completion_test-complete "somef")))
   ;; custom completion
   (should (equal "somefunction dummy "
                  (bash-completion_test-complete "somefunction du")))
   ;; function returns nothing, no -o default
   (should (equal "somefunction so"
                  (bash-completion_test-complete "somefunction so")))
   ;; function returns nothing, -o default, so fallback to default 
   (should (equal "someotherfunction some/"
                  (bash-completion_test-complete "someotherfunction so")))
   ;; wordbreak completion
   (should (equal "export SOMEPATH=some/directory:some/other/"
                  (bash-completion_test-complete
                   "export SOMEPATH=some/directory:some/oth"))))

;;; bash-completion-integration-test.el ends here
