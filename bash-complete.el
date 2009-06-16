
(require 'comint)

(defvar bash-complete-executable "bash"
  "Command-line to execute bash")
(defvar bash-complete-process-timeout 2.5)

(defvar bash-complete-process nil
  "Bash process object")
(defvar bash-complete-alist nil
  "Maps from command name to the 'complete' arguments.

For example if the following completion is defined in bash:
  complete -F _cdargs_aliases cdb
the following entry is added to `bash-complete-alist':
 (\"cdb\" . (\"-F\" \"_cdargs\"))

See `bash-complete-add-to-alist'.
")

(defun bash-complete-dynamic-complete ()
  "Bash completion function for `comint-complete-dynamic-functions'.

Call bash to do the completion."
  (when (comint-match-partial-filename)
    (unless (window-minibuffer-p (selected-window))
      (message "Bash completion..."))
    (bash-complete-dynamic-complete-0)))

(defun bash-complete-dynamic-complete-0 ()
  (save-excursion
    (let* ( (pos (point))
	    (start (comint-line-beginning-position))
	    (end (line-end-position))
	    (line (buffer-substring-no-properties start end))
	    (wordsplit (bash-complete-split start end pos))
	    (words (car wordsplit))
	    (cword (cdr wordsplit))
	    (stub (nth cword words)) )
      (comint-simple-complete stub
			      (bash-complete-comm
			       line pos words cword)))))

(defun bash-complete-join (words)
  "Join WORDS into a shell line, escaped all words with single quotes"
  (if words
      (mapconcat
       'bash-complete-quote
       words " ")
    ""))

(defun bash-complete-quote (word)
  (if (string-match "^[a-zA-Z0-9_.-]*$" word)
      word
    (concat "'" (replace-regexp-in-string "'" "\\\'" word :literal t) "'")))

(defun bash-complete-split (start end pos)
  "Split LINE like bash would do, keep track of current word at POS.

Return a list containing the words and the number of the word
at POS, the current word: ( (word1 word2 ...) . wordnum )"
  (save-excursion
    (goto-char start)
    (let ((accum (cons nil nil)))
      (setq accum (bash-complete-split-0 start end pos accum ""))
      (cons (car accum) (nreverse (cdr accum))))))

(defun bash-complete-split-0 (start end pos accum straccum)
  (let ( (char-start (char-after))
	 (quote nil) )
    (when (and char-start (or (= char-start ?') (= char-start ?\")))
      (forward-char)
      (setq quote char-start))
    (bash-complete-split-1 start end pos quote accum straccum)))

(defun bash-complete-split-1 (start end pos quote accum straccum)
  (let ((local-start (point)))
    (skip-chars-forward (bash-complete-nonsep quote) end)
    (setq straccum (concat straccum (buffer-substring-no-properties local-start (point)))))
  (cond
   ;; an escaped char, skip, whatever it is
   ((and (char-before) (= ?\\ (char-before)))
    (forward-char)
    (bash-complete-split-1
     start end pos quote
     accum
     (concat (substring straccum 0 (- (length straccum) 1))  (char-to-string (char-before)))))
   ;; opening quote
   ((and (not quote) (char-after) (or (= ?' (char-after)) (= ?\" (char-after))))
    (bash-complete-split-0 start end pos accum straccum))
   ;; closing quote
   ((and quote (char-after) (= quote (char-after)))
    (forward-char)
    (bash-complete-split-0 start end pos accum straccum))
   ;; space inside a quote
   ((and quote (char-after) (not (= quote (char-after))))
    (forward-char)
    (bash-complete-split-1
     start end pos quote accum
     (concat straccum (char-to-string (char-before)))))
   ;; word end
   (t
    (skip-chars-forward " \t\n\r" end)
    (when (> (length straccum) 0)
      (setcdr accum (cons straccum (cdr accum)))
      (when (and (not (car accum)) (> pos 0) (<= pos (point)))
	(setcar accum (- (length (cdr accum)) 1))))
    (if (< (point) end)
	(bash-complete-split-0 (point) end pos accum "")
      accum))))

(defun bash-complete-nonsep (quote)
  (if quote
      (concat "^ \t\n\r" (char-to-string quote))
    "^ \t\n\r'\""))

(defun bash-complete-comm (line pos words cword)
  "Set LINE, POS, WORDS and CWORD, call bash completion, return the result.

This function starts a separate bash process if necessary, sets up the
completion environment (COMP_LINE, COMP_POINT, COMP_WORDS, COMP_CWORD) and
calls compgen.

The result is a list of candidates, which might be empty."

  )

(defun bash-complete-require-process ()
  ;; TODO(szermatt): if this fails, kill process and complain
  (unless (bash-complete-is-running)
    (setq bash-complete-process
	  (start-process
	   "*bash-complete*"
	   "*bash-complete*"
	   bash-complete-executable
	   "--noediting"))
    (set-process-query-on-exit-flag bash-complete-process nil)
    (bash-complete-send "PS1='\v'")
    (bash-complete-send "complete -p")
    (bash-complete-send "function __bash_complete_wrapper { eval $__BASH_COMPLETE_WRAPPER }")
    (bash-complete-build-alist (process-buffer bash-complete-process)))
  bash-complete-process)

(defun bash-complete-generate-line (line pos words cword)
  (let* ( (command (file-name-nondirectory (car words)))
	  (compgen-args (cdr (assoc command bash-complete-alist))) )
    (if (not compgen-args)
	;; no custom completion. use default completion
	(bash-complete-join (list "compgen" "-o" "default" (nth cword words)))
      ;; custom completion
      (let* ( (args (copy-tree compgen-args))
	      (function (or (member "-F" args) (member "-C" args))) )
	(if function
	    (let ((function-name (car (cdr function))))
	      (setcar function "-F")
	      (setcar (cdr function) "__bash_complete_wrapper")
	      (format "__BASH_COMPLETE_WRAPPER=%s compgen %s %s"
		      (bash-complete-quote (format "COMP_LINE=%s; COMP_POS=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"$@\""
						   (bash-complete-quote line) pos cword (bash-complete-join words)
						   (bash-complete-quote function-name)))
		      (bash-complete-join args)
		      (bash-complete-quote (nth cword words))))
	  (format "compgen %s %s" (bash-complete-join args) (nth cword words)))))))

(defun bash-complete-kill-process ()
  (when (bash-complete-is-running)
    (kill-process bash-complete-process)))

(defun bash-complete-buffer ()
  (process-buffer (bash-complete-require-process)))

(defun bash-complete-is-running ()
  (and bash-complete-process (eq 'run (process-status bash-complete-process))))

(defun bash-complete-send (commandline)
  (with-current-buffer (bash-complete-buffer)
    (erase-buffer)
    (process-send-string bash-complete-process (concat commandline "\n"))
    (while (not (progn (goto-char 1) (search-forward "\v" nil t)))
      (unless (accept-process-output bash-complete-process bash-complete-process-timeout)
	(error "Timeout while waiting for an answer from bash-complete process")))
    (goto-char (point-max))
    (delete-backward-char 1)))

(defun bash-complete-build-alist (buffer)
  "Build `bash-complete-alist' with the content of BUFFER.

BUFFER should contains the output of:
  complete -p

Return `bash-complete-alist'."
  (with-current-buffer buffer
    (save-excursion
      (setq bash-complete-alist nil)
      (end-of-buffer)
      (while (= 0 (forward-line -1))
	(bash-complete-add-to-alist
	 (cdr (bash-complete-split (line-beginning-position) (line-end-position) 0))))))
  bash-complete-alist)

(defun bash-complete-add-to-alist (words)
  "Add split 'complete' line WORDS to `bash-complete-add-to-alist'.

This parses the complete command-line arguments as output by
  complete -p

This does not work on arbitrary 'complete' calls.

Lines that do not start with the word complete are skipped.

Return `bash-complete-alist'."
  (when (string= "complete" (car words))
    (let* ( (reverse-wordsrest (nreverse (cdr words)))
	    (command (car reverse-wordsrest))
	    (options (nreverse (cdr reverse-wordsrest))) )
      (when (and command options)
	(push (cons command options) bash-complete-alist))))
  bash-complete-alist)

(provide 'bash-complete)