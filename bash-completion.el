
(require 'comint)

;;if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) && -f /etc/bash_c;;ompletion ]]; then
;;  echo -n "BASH completion..."
;;  . /etc/bash_completion
;;  echo "ON"
;;fi

(defvar bash-completion-prog "bash"
  "Command-line to execute bash")

(defvar bash-completion-process-timeout 2.5)
(defvar bash-completion-initial-timeout 30
  "Timeout value to apply when talking to bash for the first time.
The first thing bash is supposed to do is process /etc/bash_complete,
which typically takes a long time.")

(defvar bash-completion-process nil
  "Bash process object")
(defvar bash-completion-alist nil
  "Maps from command name to the 'complete' arguments.

For example if the following completion is defined in bash:
  complete -F _cdargs_aliases cdb
the following entry is added to `bash-completion-alist':
 (\"cdb\" . (\"-F\" \"_cdargs\"))

See `bash-completion-add-to-alist'.
")

(defvar bash-completion-wordbreaks (append "\"'@><=;|&(:" nil))

(defun bash-completion-setup ()
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
	    'bash-completion-dynamic-complete))

;;;###autoload
(defun bash-completion-dynamic-complete ()
  "Bash completion function for `comint-complete-dynamic-functions'.

Call bash to do the completion."
  (when (not (window-minibuffer-p))
    (message "Bash completion..."))
  (let* ( (pos (point))
	  (start (bash-completion-line-beginning-position))
	  (end (line-end-position))
	  (line (buffer-substring-no-properties start end))
	  (wordsplit)
	  (cword)
	  (words)
	  (stub)
	  ;; Override configuration for comint-dynamic-simple-complete.
	  ;; Bash adds a space suffix automatically.
	  (comint-completion-addsuffix nil) )
    (save-excursion
      (setq wordsplit (bash-completion-split start end pos))
      (setq cword (car wordsplit))
      (setq words (cdr wordsplit))
      (setq stub (nth cword words)))
    (let ((completions (bash-completion-comm line (- pos start) words cword)))
      (if completions
	  (comint-dynamic-simple-complete stub completions)
	;; try default completion after a wordbreak
	(let ((after-wordbreak (bash-completion-after-last-wordbreak stub)))
	  (when (not (equal stub after-wordbreak))
	    (bash-completion-send (concat (bash-completion-cd-command-prefix) "compgen -o default -- " after-wordbreak))
	    (comint-dynamic-simple-complete after-wordbreak (bash-completion-extract after-wordbreak))))))))

(defun bash-completion-line-beginning-position (&optional start)
  (save-excursion
    (let ((start (or start (comint-line-beginning-position)))
	  (end (line-end-position)))
      (goto-char end)
      (if (search-backward-regexp "\\(;\\|\\(&&\\)\\|\\(||\\)\\)[ \t\n]" start t)
	  (match-end 0)
	start))))

(defun bash-completion-join (words)
  "Join WORDS into a shell line, escaped all words with single quotes"
  (if words
      (mapconcat
       'bash-completion-quote
       words " ")
    ""))

(defun bash-completion-quote (word)
  (if (string-match "^[a-zA-Z0-9_./-]*$" word)
      word
    (concat "'"
	    (replace-regexp-in-string "'" "'\\''" word :literal t)
	    "'")))

(defun bash-completion-escape (word)
  (message "escape: %s" word)
  (if (string-match "^['\"]" word)
      word
    (replace-regexp-in-string "\\([ '\"]\\)" "\\\\\\1" word)))

(defun bash-completion-split (start end pos)
  "Split LINE like bash would do, keep track of current word at POS.

Return a list containing the words and the number of the word
at POS, the current word: ( (word1 word2 ...) . wordnum )"
  (save-excursion
    (goto-char start)
    (let ((accum (cons nil nil)))
      (setq accum (bash-completion-split-0 start end pos accum ""))
      (when (and (not (null pos)) (null (car accum)))
	(setcar accum (length (cdr accum)))
	(setcdr accum (cons "" (cdr accum))))
      (cons (car accum) (nreverse (cdr accum))))))

(defun bash-completion-split-0 (start end pos accum straccum)
  (when (eq "" straccum)
    (let ((local-start (point)))
      (when (and (null (car accum)) (not (null pos)) (<= pos local-start))
	(setcar accum (length (cdr accum)))
	(setcdr accum (cons "" (cdr accum))))))
  (let ( (char-start (char-after))
	 (quote nil) )
    (when (and char-start (or (= char-start ?') (= char-start ?\")))
      (forward-char)
      (setq quote char-start))
    (bash-completion-split-1 start end pos quote accum straccum)))

(defun bash-completion-split-1 (start end pos quote accum straccum)
  (let ((local-start (point)))
    (skip-chars-forward (bash-completion-nonsep quote) end)
    (setq straccum (concat straccum (buffer-substring-no-properties local-start (point)))))
  (cond
   ;; an escaped char, skip, whatever it is
   ((and (char-before) (= ?\\ (char-before)))
    (forward-char)
    (bash-completion-split-1
     start end pos quote
     accum
     (concat (substring straccum 0 (- (length straccum) 1))  (char-to-string (char-before)))))
   ;; opening quote
   ((and (not quote) (char-after) (or (= ?' (char-after)) (= ?\" (char-after))))
    (bash-completion-split-0 start end pos accum straccum))
   ;; closing quote
   ((and quote (char-after) (= quote (char-after)))
    (forward-char)
    (bash-completion-split-0 start end pos accum straccum))
   ;; space inside a quote
   ((and quote (char-after) (not (= quote (char-after))))
    (forward-char)
    (bash-completion-split-1
     start end pos quote accum
     (concat straccum (char-to-string (char-before)))))
   ;; word end
   (t
    (when (and (null (car accum)) (not (null pos)) (<= pos (point)))
      (setcar accum (length (cdr accum))))
    (skip-chars-forward " \t\n\r" end)
    (when (> (length straccum) 0)
      (setcdr accum (cons straccum (cdr accum))))
    (if (< (point) end)
	(bash-completion-split-0 (point) end pos accum "")
      accum))))

(defun bash-completion-nonsep (quote)
  (if quote
      (concat "^ \t\n\r" (char-to-string quote))
    "^ \t\n\r'\""))

(defun bash-completion-comm (line pos words cword)
  "Set LINE, POS, WORDS and CWORD, call bash completion, return the result.

This function starts a separate bash process if necessary, sets up the
completion environment (COMP_LINE, COMP_POINT, COMP_WORDS, COMP_CWORD) and
calls compgen.

The result is a list of candidates, which might be empty."
  (bash-completion-send (concat (bash-completion-generate-line line pos words cword) " 2>/dev/null"))
  (bash-completion-extract (nth cword words)))

(defun bash-completion-extract (stub)
  (let ((bash-completion-prefix stub))
    (mapcar 'bash-completion-fix
	    (with-current-buffer (bash-completion-buffer)
	      (split-string (buffer-string) "\n" t)))))

(defun bash-completion-fix (str &optional prefix)
  (let ((prefix (or prefix bash-completion-prefix))
	(suffix ""))
    (bash-completion-addsuffix 
     (let* ((rebuilt)
	    (rest (cond
		   ((bash-completion-starts-with str prefix)
		    (substring str (length prefix)))
		   ;; bash expands the home directory automatically. This is confusing
		   ;; for comint-dynamic-simple-complete
		   ((and (bash-completion-starts-with prefix "~")
			 (bash-completion-starts-with str (expand-file-name "~")))
		    (substring (concat "~" (substring str (length (expand-file-name "~"))))
			       (length prefix)))
		   ;; bash sometimes just prints whatever needs to be expanded,
		   ;; for example: "export PATH=<complete>". Prepend the old
		   ;; prefix to avoid confusing comint-dynamic-simple-complete
		   ((bash-completion-starts-with 
		     (setq rebuilt (concat (bash-completion-before-last-wordbreak prefix) str))
		     prefix)
		    (substring rebuilt (length prefix)))
		   (t str))))
       (when (bash-completion-ends-with rest " ")
	 (setq rest (substring rest 0 -1))
	 (setq suffix " "))
       (concat prefix (bash-completion-escape rest) suffix)))))

(defun bash-completion-before-last-wordbreak (str)
  (car (bash-completion-last-wordbreak-split str)))

(defun bash-completion-after-last-wordbreak (str)
  (cdr (bash-completion-last-wordbreak-split str)))

(defun bash-completion-last-wordbreak-split (str)
  (catch 'bash-completion-return
    (let ((end (- (length str) 1)))
      (while (> end 0)
	(when (memq (aref str end) bash-completion-wordbreaks)
	  (throw 'bash-completion-return (cons (substring str 0 (1+ end)) (substring str (1+ end)))))
	(setq end (1- end))))
      (cons "" str)))

(defun bash-completion-ends-with (str suffix)
  (let ((suffix-len (length suffix))
	(str-len (length str)))
    (and
     (>= str-len suffix-len)
     (equal (substring str (- suffix-len)) suffix))))
  
(defun bash-completion-starts-with (str prefix)
  (let ((prefix-len (length prefix))
	(str-len (length str)))
    (and
     (>= str-len prefix-len)
     (equal (substring str 0 prefix-len) prefix))))

(defun bash-completion-addsuffix (str)
  (if (and (null (string-match "[/: ]$" str))
	   (file-accessible-directory-p (expand-file-name str default-directory)))
      (progn
	(concat str "/"))
    str))

(defun bash-completion-require-process ()
  (if (bash-completion-is-running)
      bash-completion-process
    ;; start process
    (let ((process))
      (unwind-protect
	  (progn
	    (setenv "EMACS_BASH_COMPLETE" "t")
	    (setq process
		  (start-process
		   "*bash-completion*"
		   "*bash-completion*"
		   bash-completion-prog
		   "--noediting"))
	    (set-process-query-on-exit-flag process nil)
	    (let* ((shell-name (file-name-nondirectory bash-completion-prog))
		   (startfile1 (concat "~/.emacs_" shell-name ".sh"))
		   (startfile2 (concat "~/.emacs.d/init_" shell-name ".sh")))
	      (cond
	       ((file-exists-p startfile1)
		(process-send-string process (concat ". " startfile1 "\n")))
	       ((file-exists-p startfile2)
		(process-send-string process (concat ". " startfile2 "\n")))))
	    (bash-completion-send "PS1='\v'" process bash-completion-initial-timeout)
	    (bash-completion-send "function __bash_complete_wrapper { eval $__BASH_COMPLETE_WRAPPER; }" process)
	    ;; some bash completion functions use quote_readline to double-quote
	    ;; strings - which compgen understands but only in some environment.
	    ;; disable this dreadful business to get a saner way of handling
	    ;; spaces.
	    (bash-completion-send "function quote_readline { echo \"$1\"; }" process)
	    (bash-completion-send "complete -p" process)
	    (bash-completion-build-alist (process-buffer process))
	    (setq bash-completion-process process)
	    (setq process nil)
	    bash-completion-process)
	;; finally
	(progn
	  (setenv "EMACS_BASH_COMPLETE" nil)
	  (when process
	    (condition-case err
		(kill-process process)
	      (error nil))))))))

(defun bash-completion-cd-command-prefix ()
  (if default-directory
      (concat "cd 2>/dev/null " (bash-completion-quote (expand-file-name default-directory)) " ; ") 
    ""))

(defun bash-completion-generate-line (line pos words cword)
  (concat
   (bash-completion-cd-command-prefix)
   (let* ( (command-name (file-name-nondirectory (car words)))
	   (compgen-args (cdr (assoc command-name bash-completion-alist))) )
     (if (not compgen-args)
	 ;; no custom completion. use default completion
	 (if (= cword 0)
	     ;; a command. let emacs expand executable, let bash expand builtins, aliases and functions
	     (concat (bash-completion-join (list "compgen" "-S" " " "-b" "-a" "-A" "function" (car words))))
	   ;; argument
	   (bash-completion-join (list "compgen" "-o" "default" (nth cword words))))
       ;; custom completion
       (let* ( (args (copy-tree compgen-args))
	       (function (or (member "-F" args) (member "-C" args))) )
	 (if function
	     (let ((function-name (car (cdr function))))
	       (setcar function "-F")
	       (setcar (cdr function) "__bash_complete_wrapper")
	       (format "__BASH_COMPLETE_WRAPPER=%s compgen %s -- %s"
		       (bash-completion-quote (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
						    (bash-completion-quote line) pos cword (bash-completion-join words)
						    (bash-completion-quote function-name)))
		       (bash-completion-join args)
		       (bash-completion-quote (nth cword words))))
	   (format "compgen %s -- %s" (bash-completion-join args) (nth cword words))))))))

(defun bash-completion-reset ()
  (interactive)
  (when (bash-completion-is-running)
    (kill-process bash-completion-process))
  (setq bash-completion-process nil))

(defun bash-completion-buffer ()
  (process-buffer (bash-completion-require-process)))

(defun bash-completion-is-running ()
  (and bash-completion-process (eq 'run (process-status bash-completion-process))))

(defun bash-completion-send (commandline &optional process timeout)
  ;;(message commandline)
  (let ((process (or process (bash-completion-require-process)))
	(timeout (or timeout bash-completion-process-timeout)))
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (process-send-string process (concat commandline "\n"))
      (while (not (progn (goto-char 1) (search-forward "\v" nil t)))
	(unless (accept-process-output process timeout)
	  (error "Timeout while waiting for an answer from bash-completion process")))
      (goto-char (point-max))
      (delete-backward-char 1))))

(defun bash-completion-build-alist (buffer)
  "Build `bash-completion-alist' with the content of BUFFER.

BUFFER should contains the output of:
  complete -p

Return `bash-completion-alist'."
  (with-current-buffer buffer
    (save-excursion
      (setq bash-completion-alist nil)
      (goto-char (point-max))
      (while (= 0 (forward-line -1))
	(bash-completion-add-to-alist
	 (cdr (bash-completion-split (line-beginning-position) (line-end-position) nil))))))
  bash-completion-alist)

(defun bash-completion-add-to-alist (words)
  "Add split 'complete' line WORDS to `bash-completion-add-to-alist'.

This parses the complete command-line arguments as output by
  complete -p

This does not work on arbitrary 'complete' calls.

Lines that do not start with the word complete are skipped.

Return `bash-completion-alist'."
  (when (string= "complete" (car words))
    (let* ( (reverse-wordsrest (nreverse (cdr words)))
	    (command (car reverse-wordsrest))
	    (options (nreverse (cdr reverse-wordsrest))) )
      (when (and command options)
	(push (cons command options) bash-completion-alist))))
  bash-completion-alist)

(provide 'bash-completion)
