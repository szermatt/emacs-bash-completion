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
  `(progn
     (let ((test-env-dir (bash-completion_test-setup-env)))
       (let ((bash-completion-processes nil)
             (bash-completion-alist nil)
             (bash-completion-nospace nil)
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

(defmacro bash-completion_test-with-shell (complete-me)
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
	      (insert ,complete-me)
              (let ((comint-dynamic-complete-functions '(bash-completion-dynamic-complete)))
                (completion-at-point))
	      (buffer-substring-no-properties
               (comint-line-beginning-position) (point))))
	;; finally
	(when (and shell-buffer (buffer-live-p shell-buffer))
	  (kill-process (get-buffer-process shell-buffer))
	  (kill-buffer shell-buffer))))))

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
        (insert "function somefunction { echo ok; }\n"))
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
  (let ((bash-completion-enable-caching nil))
    (bash-completion_test-integration)))

(ert-deftest bash-completion-integration-test-with-caching ()
  (let ((bash-completion-enable-caching t))
    (bash-completion_test-integration)))

(defun bash-completion_test-integration ()
  (if (file-executable-p bash-completion-prog)
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
       (should-not (bash-completion-is-running)))))

(ert-deftest bash-completion-integration-setenv-test ()
  (let ((bash-completion-enable-caching nil))
    (bash-completion_test-integration-setenv-test)))

(ert-deftest bash-completion-integration-setenv-test-with-caching ()
  (let ((bash-completion-enable-caching t))
    (bash-completion_test-integration-setenv-test)))

(defun bash-completion_test-integration-setenv-test ()
  (if (file-executable-p bash-completion-prog)
      (bash-completion_test-harness
       (bash-completion-send "echo $EMACS_BASH_COMPLETE")
       (with-current-buffer (bash-completion-buffer)
         (should (equal "t\n" (buffer-string)))))))

(ert-deftest bash-completion-integration-one-completion-test ()
  (let ((bash-completion-enable-caching nil))
    (bash-completion_test-integration-one-completion-test)))

(ert-deftest bash-completion-integration-one-completion-test-with-caching ()
  (let ((bash-completion-enable-caching t))
    (bash-completion_test-integration-one-completion-test)))

(defun bash-completion_test-integration-one-completion-test ()
  (if (file-executable-p bash-completion-prog)
      (should (equal "somefunction "
                     (bash-completion_test-with-shell "somef")))))

(ert-deftest bash-completion-integration-wordbreak-completion-test ()
  (let ((bash-completion-enable-caching nil))
    (bash-completion_test-integration-wordbreak-completion-test)))

(ert-deftest bash-completion-integration-wordbreak-completion-test-with-caching ()
  (let ((bash-completion-enable-caching t))
    (bash-completion_test-integration-wordbreak-completion-test)))

(defun bash-completion_test-integration-wordbreak-completion-test ()
  (if (file-executable-p bash-completion-prog)
      (should (equal "export SOMEPATH=some/directory:some/other/"
                     (bash-completion_test-with-shell
                      "export SOMEPATH=some/directory:some/oth")))))


;;; bash-completion-integration-test.el ends here
