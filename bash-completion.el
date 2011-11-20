;;; bash-completion.el --- BASH completion for the shell buffer

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
;; This file defines dynamic completion hooks for shell-mode and
;; shell-command prompts that are based on bash completion.
;;
;; You will need shell-command.el to get tab completion in the
;; minibuffer. See http://www.namazu.org/~tsuchiya/elisp/shell-command.el
;;
;; Bash completion for emacs:
;; - is aware of bash builtins, aliases and functions
;; - does file expansion inside of colon-separated variables
;;   and after redirections (> or <)
;; - escapes special characters when expanding file names
;; - is configurable through programmable bash completion
;;
;; When the first completion is requested in shell model or a shell
;; command, bash-completion.el starts a separate bash
;; process.  Bash-completion.el then uses this process to do the actual
;; completion and includes it into Emacs completion suggestions.
;;
;; A simpler and more complete alternative to bash-completion.el is to
;; run a bash shell in a buffer in term mode(M-x `ansi-term').
;; Unfortunately, many Emacs editing features are not available when
;; running in term mode.  Also, term mode is not available in
;; shell-command prompts.
;;
;; INSTALLATION
;;
;; 1. copy bash-completion.el into a directory that's on Emacs load-path
;; 2. add this into your .emacs file:
;;   (autoload 'bash-completion-dynamic-complete \"bash-completion\"
;;     \"BASH completion hook\")
;;   (add-hook 'shell-dynamic-complete-functions
;; 	'bash-completion-dynamic-complete)
;;   (add-hook 'shell-command-complete-functions
;; 	'bash-completion-dynamic-complete)
;;
;;   or simpler, but forces you to load this file at startup:
;; 
;;   (require 'bash-completion)
;;   (bash-completion-setup)
;;
;; 3. reload your .emacs (M-x `eval-buffer') or restart
;;
;; Once this is done, use <TAB> as usual to do dynamic completion from
;; shell mode or a shell command minibuffer, such as the one started
;; for M-x `compile'. Note that the first completion is slow, as emacs
;; launches a new bash process.
;;
;; You'll get better results if you turn on programmable bash completion.
;; On Ubuntu, this means running:
;;   sudo apt-get install bash-completion
;; and then adding this to your .bashrc:
;;   . /etc/bash_completion
;;
;; Right after enabling programmable bash completion, and whenever you
;; make changes to you .bashrc, call `bash-completion-reset' to make
;; sure bash completion takes your new settings into account.
;;
;; Loading /etc/bash_completion often takes time, and is not necessary
;; in shell mode, since completion is done by a separate process, not
;; the process shell-mode process.
;;
;; To turn off bash completion when running from emacs but keep it on
;; for processes started by bash-completion.el, add this to your .bashrc:
;; if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
;;      -f /etc/bash_completion ]]; then
;;   . /etc/bash_completion
;; fi
;;
;; Emacs sets the environment variable INSIDE_EMACS to the processes
;; started from it. Processes started by bash-completion.el have
;; the environment variable EMACS_BASH_COMPLETE set to t.
;;
;; CAVEATS
;;
;; Using a separate process for doing the completion has several
;; important disadvantages:
;; - bash completion is slower than standard emacs completion
;; - the first completion can take a long time, since a new bash process
;;   needs to be started and initialized
;; - the separate process is not aware of any changes made to bash
;;   in the current buffer.
;;   In a standard terminal, you could do:
;;     $ alias myalias=ls
;;     $ myal<TAB>
;;   and bash would propose the new alias.
;;   Bash-completion.el cannot do that, as it is not aware of anything
;;   configured in the current shell. To make bash-completion.el aware
;;   of a new alias, you need to add it to .bashrc and restart the
;;   completion process using `bash-completion-reset'.
;;
;; COMPATIBILITY
;;
;; bash-completion.el is quite sensitive to the OS and BASH version.
;; This package is known to work on the following environment:
;;   GNU Emacs 22.3.1 (Aquamacs 1.7)
;;   GNU Emacs 22.1.1 (OSX 10.5)
;;   GNU Emacs 22.1.1 (Ubuntu 8.04)
;;   GNU Emacs 23.0.94.1 (Ubuntu 8.10)
;;
;; and using the following bash versions:
;;   BASH 3.2.17
;;   BASH 3.2.32
;;   BASH 3.2.39
;;
;; bash-completion.el does not works on XEmacs.
;;

;;; History:
;;
;; 2009-11-25   Stephane Zermatten <szermatt@gmail.com>
;;
;; * bash-completion-require-process: set MAILCHECK to -1
;; to disable mail check message. 
;;
;; 2009-08-01   Stephane Zermatten <szermatt@gmail.com>
;;
;; * bash-completion-generate-line: add missing compgen 
;; option to complete commands (duh!).
;;
;; Current version:
;; $Id$
;;

(require 'comint)

;;; Code:

;;; ---------- Customization
(defgroup bash-completion nil
  "BASH configurable command-line completion "
  :group 'shell
  :group 'shell-command)

(defcustom bash-completion-enabled t
  "Enable/Disable BASH configurable command-line completion globally.

This flag is useful for temporarily disabling bash completion
once it's been installed.

Setting this variable to t is NOT enough to enable BASH completion.
BASH completion is only available in the environment for which
`bash-completion-dynamic-complete' has been registered. See
`bash-completion-setup' for that."
  :type '(boolean)
  :group 'bash-completion)

(defcustom bash-completion-prog "/bin/bash"
  "Name or path of the BASH executable to run for command-line completion.
This should be either an absolute path to the BASH executable or
the name of the bash command if it is on Emacs' PATH.  This
should point to a recent version of BASH (BASH 3) with support
for command-line completion."
  :type '(file :must-match t)
  :group 'bash-completion)

(defcustom bash-completion-process-timeout 2.5
  "Timeout value to apply when waiting from an answer from bash.
If bash takes longer than that to answer, the answer will be
ignored."
  :type '(float)
  :group 'bash-completion)

(defcustom bash-completion-initial-timeout 30
  "Timeout value to apply when talking to bash for the first time.
The first thing bash is supposed to do is process /etc/bash_complete,
which typically takes a long time."
  :type '(float)
  :group 'bash-completion)

(defcustom bash-completion-nospace nil
  "Never let bash add a final space at the end of a completion.

When there is only one completion candidate, bash sometimes adds
a space at the end of the completion to move the cursor at the
appropriate position to add more command-line arguments. This
feature doesn't always work perfectly with programmable completion.

Enable this option if you find yourself having to often backtrack
to remove the extra space bash adds after a completion."
  :type '(boolean)
  :group 'bash-completion)

;;; ---------- Internal variables and constants

(defvar bash-completion-process nil
  "Bash process object.")
(defvar bash-completion-alist nil
  "Maps from command name to the 'complete' arguments.

For example if the following completion is defined in bash:
  complete -F _cdargs_aliases cdb
the following entry is added to `bash-completion-alist':
 (\"cdb\" . (\"-F\" \"_cdargs\"))

See `bash-completion-add-to-alist'.")

(defconst bash-completion-wordbreaks-str "\"'@><=;|&(:"
  "String of word break characters.
This is the equivalent of COMP_WORDBREAKS: special characters
that are considered word breaks in some cases when doing
completion.  This was introduced initially to support file
completion in colon-separated values.")

(defconst bash-completion-wordbreaks
  (append bash-completion-wordbreaks-str nil)
  "`bash-completion-wordbreaks-str' as a list of characters.")

;;; ---------- Functions: completion

;;;###autoload
(defun bash-completion-setup ()
  "Register bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions' and to the
completion function list of shell-command, `shell-command-complete-functions'.

This function is convenient, but it might not be the best way of enabling
bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

  (autoload 'bash-completion-dynamic-complete \"bash-completion\"
    \"BASH completion hook\")
  (add-hook 'shell-dynamic-complete-functions
  	  'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
  	  'bash-completion-dynamic-complete))
"
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
	    'bash-completion-dynamic-complete))

;;;###autoload
(defun bash-completion-dynamic-complete ()
  "Complete word at cursor using BASH completion.

This function is meant to be added into
`shell-dynamic-complete-functions' or
`shell-command-complete-functions'.  It uses `comint' to figure
out what the current command is and calls
`comint-dynamic-simple-complete' to do the completion.

If a match was found, it is displayed as is usual for comint
completion.  Return nil if no match was found."
  (when bash-completion-enabled
    (when (not (window-minibuffer-p))
      (message "Bash completion..."))
    (let* ( (start (comint-line-beginning-position))
	    (pos (point))
	    (tokens (bash-completion-tokenize start pos))
	    (open-quote (bash-completion-tokenize-open-quote tokens))
	    (parsed (bash-completion-process-tokens tokens pos))
	    (line (cdr (assq 'line parsed)))
	    (point (cdr (assq 'point parsed)))
	    (cword (cdr (assq 'cword parsed)))
	    (words (cdr (assq 'words parsed)))
	    (stub (nth cword words))
	    (completions (bash-completion-comm line point words cword open-quote))
	    ;; Override configuration for comint-dynamic-simple-complete.
	    ;; Bash adds a space suffix automatically.
	    (comint-completion-addsuffix nil) )
      (if completions
	  (comint-dynamic-simple-complete stub completions)
	;; no standard completion
	;; try default (file) completion after a wordbreak
	(bash-completion-dynamic-try-wordbreak-complete stub open-quote)))))

(defun bash-completion-dynamic-try-wordbreak-complete (stub open-quote)
  "Try wordbreak completion on STUB if the complete completion failed.

Split STUB using the wordbreak list and apply compgen default
completion on the last part.  Return non-nil if a match was found.

If STUB is quoted, the quote character, ' or \", should be passed
to the parameter OPEN-QUOTE.

This function is not meant to be called outside of
`bash-completion-dynamic-complete'."
  (let* ((wordbreak-split (bash-completion-last-wordbreak-split stub))
	 (before-wordbreak (car wordbreak-split))
	 (after-wordbreak (cdr wordbreak-split)))
    (when (car wordbreak-split)
      (bash-completion-send (concat
			     (bash-completion-cd-command-prefix)
			     "compgen -o default -- "
			     (bash-completion-quote after-wordbreak)))
      (comint-dynamic-simple-complete
       after-wordbreak
       (bash-completion-extract-candidates after-wordbreak open-quote)))))

;;; ---------- Functions: parsing and tokenizing

(defun bash-completion-join (words)
  "Join WORDS into a shell command line.

All words that contain even mildly suspicious characters are
quoted using single quotes to avoid the shell interpreting them
when it shouldn't.

Return one string containing WORDS."
  (if words
      (mapconcat
       'bash-completion-quote
       words " ")
    ""))

(defun bash-completion-quote (word)
  "Put single quotes around WORD unless it's crearly unnecessary.

If WORD contains characters that aren't known to be harmless, this
functions adds single quotes around it and return the result."
  (if (string-match "^[a-zA-Z0-9_./-]*$" word)
      word
    (concat "'"
	    (replace-regexp-in-string "'" "'\\''" word :literal t)
	    "'")))

(defun bash-completion-parse-line (start pos)
  "Split a command line in the current buffer between START and POS.

This function combines `bash-completion-tokenize' and
`bash-completion-process-tokens'.  It takes the same arguments as
`bash-completion-tokenize' and returns the same value as
`bash-completion-process-tokens'."
  (bash-completion-process-tokens
   (bash-completion-tokenize start pos) pos))

(defun bash-completion-process-tokens (tokens pos)
  "Process a command line split into TOKENS that end at POS.

This function takes a list of tokens built by
`bash-completion-tokenize' and returns the variables compgen
function expect in an association list.

Return an association list with the current symbol as keys:
 line - the relevant command between START and POS (string)
 point - position of the cursor in line (number)
 words - line split into words, unescaped (list of strings)
 cword - 0-based index of the word to be completed in words (number)"
  (bash-completion-parse-line-postprocess
   (bash-completion-parse-current-command tokens) pos))

(defun bash-completion-parse-line-postprocess (tokens pos)
  "Extract from TOKENS the data needed by compgen functions.

This function takes a list of TOKENS created by `bash-completion-tokenize'
for the current buffer and generate the data needed by compgen functions
as returned by `bash-completion-parse-line' given the cursor position POS."
  (let* ((first-token (car tokens))
	 (last-token (car (last tokens)))
	 (start (or (car (bash-completion-tokenize-get-range first-token)) pos))
	 (end (or (cdr (bash-completion-tokenize-get-range last-token)) pos))
	 (words (bash-completion-strings-from-tokens tokens)))
    (when (or (> pos end) (= start end))
      (setq words (append words '(""))))
    (list
     (cons 'line (buffer-substring-no-properties start pos))
     (cons 'point (- pos start))
     (cons 'cword (- (length words) 1))
     (cons 'words words))))

(defun bash-completion-parse-current-command (tokens)
  "Extract from TOKENS the tokens forming the current command.

This function takes a list of TOKENS created by
`bash-completion-tokenize' for the current buffer and select the
tokens on this list that form the current command given that the
word to be completed is the last token.

For example, given this stream of tokens:
  cd /var/tmp && ls -l
if the last token is -l, it will select:
  ls -l
if the last token is /var/tmp, it will select:
  cd /var/tmp

Return a sublist of TOKENS."
  (nreverse
   (catch 'bash-completion-return
     (let ((command nil) (state 'initial))
       (dolist (token tokens)
	 (let* (
		(string (bash-completion-tokenize-get-str token))
		(is-terminal
		 (and (member string '(";" "&" "|" "&&" "||"))
		      (let ((range (bash-completion-tokenize-get-range token)))
			(= (- (cdr range) (car range))
			   (length string))))))
	   (cond
	    (is-terminal
	     (setq state 'initial)
	     (setq command nil))

	    ((and (eq state 'initial)
		  (null (string-match "=" string)))
	     (setq state 'args)
	     (push token command))

	    ((eq state 'args)
	     (push token command)))))
       (or command (last tokens))))))

(defun bash-completion-strings-from-tokens (tokens)
  "Extract the strings from TOKENS.

This function takes all strings from TOKENS and retrun it as a
list of strings.

TOKENS should be in the format returned by `bash-completion-tokenize'."
  (mapcar 'bash-completion-tokenize-get-str tokens))

(defsubst bash-completion-tokenize-get-range (token)
  "Return the TOKEN range as a cons: (start . end)."
  (cdr (assq 'range token)))

(defsubst bash-completion-tokenize-set-end (token)
  "Set the end position of TOKEN to the cursor position."
  (setcdr (bash-completion-tokenize-get-range token) (point)))

(defsubst bash-completion-tokenize-append-str (token str)
  "Append to TOKEN the string STR."
  (let ((str-cons (assq 'str token)))
    (setcdr str-cons (concat (cdr str-cons) str))))

(defsubst bash-completion-tokenize-get-str (token)
  "Return the TOKEN string."
  (cdr (assq 'str token)))

(defsubst bash-completion-tokenize-open-quote (tokens)
  "Return the quote character that was still open in the last token.

TOKENS is a list of token as returned by
`bash-completion-tokenize'."
  (cdr (assq 'quote (car (last tokens)))))

(defun bash-completion-tokenize (start end)
  "Tokenize the portion of the current buffer between START and END.

This function splits a BASH command line into tokens.  It knows
about quotes, escape characters and special command separators such
as ;, | and &&.

This method returns a list of tokens found between START and END,
ordered by position.  Tokens contain a string and a range.

The string in a token is an unescaped version of the token.  For
example, if the token is 'hello world', the string contains
\"hello world\", without the quotes.  It can be accessed using
`bash-completion-tokenize-get-str'.  It can be modified using
`bash-completion-tokenize-append-str'.

The range is a cons containing the start and end position of the
token (start . end).  Start is the position of the first character
that belongs to the token.  End is the position of the first
character that doesn't belong to the token.  For example in the
string \" hello world \", the first token range is (2 . 7) and
the second token range (9 . 14). It can be accessed using
`bash-completion-tokenize-get-range'. The end position can be
set using `bash-completion-tokenize-set-end'.

Tokens should always be accessed using the functions specified above,
never directly as they're likely to change as this code evolves.
The current format of a token is '(string . (start . end))."
  (save-excursion
    (goto-char start)
    (nreverse (bash-completion-tokenize-new-element end nil))))

(defun bash-completion-tokenize-new-element (end tokens)
  "Tokenize the rest of the line until END and complete TOKENS.

This function is meant to be called exclusively from
`bash-completion-tokenize' and `bash-completion-tokenize-0'.

This function expect the point to be at the start of a new
element to be added to the list of tokens.

Return TOKENS with new tokens found betwen the current point and
END prepended to it."
  (skip-chars-forward " \t\n\r" end)
  (if (< (point) end)
      (bash-completion-tokenize-0 end tokens
				  (list
				   (cons 'str "")
				   (cons 'range (cons (point) nil))))
    tokens))

(defun bash-completion-tokenize-0 (end tokens token)
  "Tokenize the rest of the token until END and add it into TOKENS.

This function is meant to be called exclusively from
`bash-completion-tokenize-new-element'.

This function expect the point to be at the start of a new token
section, either at the start of the token or just after a quote
has been closed in the token.  It detects new opening quotes and
calls `bash-completion-tokenize-1'.

END specifies the point at which tokenization should stop.

TOKENS is the list of tokens built so farin reverse order.

TOKEN is the token currently being built.

Return TOKENS with new tokens prepended to it."
  (let ( (char-start (char-after))
	 (quote nil) )
    (when (and char-start (or (= char-start ?') (= char-start ?\")))
      (forward-char)
      (setq quote char-start))
    (bash-completion-tokenize-1 end quote tokens token)))

(defun bash-completion-tokenize-1 (end quote tokens token)
  "Tokenize the rest of the token.

This function is meant to be called exclusively from
`bash-completion-tokenize-0'.

This function tokenize the rest of the token and either call
itself and `bash-completion-tokenize-0' recursively or append the
token to the list of token and call
`bash-completion-tokenize-new-element' to look for the next
token.

END specifies the point at which tokenization should stop.

QUOTE specifies the current quote.  It should be nil ?' or ?\"

TOKENS is the list of tokens built so farin reverse order.

TOKEN is the token currently being built.

Return TOKENS with new tokens prepended to it."
  ;; parse the token elements at the current position and
  ;; append them
  (let ((local-start (point)))
    (when (= (skip-chars-forward "[;&|]" end) 0)
      (skip-chars-forward (bash-completion-nonsep quote) end))
    (bash-completion-tokenize-append-str
     token
     (buffer-substring-no-properties local-start (point))))
  (cond
   ;; an escaped char, skip, whatever it is
   ((and (char-before) (= ?\\ (char-before)))
    (forward-char)
    (let ((str (bash-completion-tokenize-get-str token)))
      (aset str (1- (length str)) (char-before)))
    (bash-completion-tokenize-1 end quote tokens token))
   ;; opening quote
   ((and (not quote) (char-after) (or (= ?' (char-after)) (= ?\" (char-after))))
    (bash-completion-tokenize-0 end tokens token))
   ;; closing quote
   ((and quote (char-after) (= quote (char-after)))
    (forward-char)
    (bash-completion-tokenize-0 end tokens token))
   ;; space inside a quote
   ((and quote (char-after) (not (= quote (char-after))))
    (forward-char)
    (bash-completion-tokenize-append-str token (char-to-string (char-before)))
    (bash-completion-tokenize-1 end quote tokens token))
   ;; word end
   (t
    (bash-completion-tokenize-set-end token)
    (when quote
      (push (cons 'quote quote) token))
    (push token tokens)
    (bash-completion-tokenize-new-element end tokens))))

(defconst bash-completion-nonsep-alist
  '((nil . "^ \t\n\r;&|'\"#")
    (?' . "^ \t\n\r'")
    (?\" . "^ \t\n\r\""))
  "Alist of sets of non-breaking characters.
Keeps a regexp specifying the set of non-breaking characters for
all quoting environment (no quote, single quote and double
quote).  Get it using `bash-completion-nonsep'.")

(defun bash-completion-nonsep (quote)
  "Return the set of non-breaking characters when QUOTE is the current quote.

QUOTE should be nil, ?' or ?\"."
  (cdr (assq quote bash-completion-nonsep-alist)))

;;; ---------- Functions: getting candidates from bash

(defun bash-completion-comm (line pos words cword open-quote)
  "Set LINE, POS, WORDS and CWORD, call compgen, return the result.

This function starts a separate bash process if necessary, sets
up the completion environment (COMP_LINE, COMP_POINT, COMP_WORDS,
COMP_CWORD) and calls compgen.

OPEN-QUOTE should be the quote, a character, that's still open in
the last word or nil.

The result is a list of candidates, which might be empty."
  ;; start process now, to make sure bash-completion-alist is
  ;; set before we run bash-completion-generate-line
  (bash-completion-require-process)
  (bash-completion-send
   (concat
    (bash-completion-generate-line line pos words cword)
    " 2>/dev/null"))
  (bash-completion-extract-candidates (nth cword words) open-quote))

(defun bash-completion-extract-candidates (stub open-quote)
  "Extract the completion candidates from the process buffer for STUB.

This command takes the content of the completion process buffer, split
it by newlines, post-process the candidates and returns them as a list
of strings.

It should be invoked with the comint buffer as the current buffer
for directory name detection to work.

If STUB is quoted, the quote character, ' or \", should be passed
in OPEN-QUOTE.

Post-processing includes escaping special characters, adding a /
to directory names, merging STUB with the result.  See `bash-completion-fix'
for more details."
  (let ((bash-completion-prefix stub)
	(bash-completion-open-quote open-quote))
    (mapcar 'bash-completion-fix
	    (with-current-buffer (bash-completion-buffer)
	      (split-string (buffer-string) "\n" t)))))

(defun bash-completion-fix (str &optional prefix open-quote)
  "Fix completion candidate in STR if PREFIX is the current prefix.

STR is the completion candidate to modify.

PREFIX should be the current string being completed.  If it is
nil, the value of `bash-completion-prefix' is used.  This allows
calling this function from `mapcar'.

OPEN-QUOTE should be the quote that's still open in prefix.  A
character (' or \") or nil.  If it is nil, the value of
`bash-completion-open-quote' is used.  This allows
calling this function from `mapcar'.

Return a modified version of the completion candidate.

Modification include:
 - escaping of special characters in STR
 - prepending PREFIX if STR does not contain all of it, when
   completion was done after a wordbreak
 - adding / to recognized directory names

It should be invoked with the comint buffer as the current buffer
for directory name detection to work."
  (let ((prefix (or prefix bash-completion-prefix))
	(open-quote (or open-quote (and (boundp 'bash-completion-open-quote) bash-completion-open-quote)))
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
		   ((bash-completion-starts-with prefix str)
		    ;; completion is a substring of prefix something's
		    ;; gone wrong. Treat it as one (useless)
		    ;; candidate.
		    (setq prefix "")
		    (setq rest str))
		   ;; completion sometimes only applies to the last word, as
		   ;; defined by COMP_WORDBREAKS. This detects and works around
		   ;; this feature.
		   ((bash-completion-starts-with
		     (setq rebuilt (concat (bash-completion-before-last-wordbreak prefix) str))
		     prefix)
		    (substring rebuilt (length prefix)))
		   ;; there is no meaningful link between the prefix and
		   ;; the string. just append the string to the prefix and
		   ;; hope for the best.
		   (t str))))
       (when (bash-completion-ends-with rest " ")
	 (setq rest (substring rest 0 -1))
	 (unless bash-completion-nospace
	     (setq suffix " ")))
       (concat prefix (bash-completion-escape-candidate rest open-quote) suffix)))))

(defun bash-completion-escape-candidate (completion-candidate open-quote)
  "Escapes COMPLETION-CANDIDATE.

This function escapes all special characters in the result of
bash completion.  It does nothing if COMPLETION-CANDIDATE looks
like a quoted string.

It uses escape characters appropriate for the quote defined in
OPEN-QUOTE, either nil, ' or \".

Return a possibly escaped version of COMPLETION-CANDIDATE."
  (cond
   ((and (null open-quote)
	 (null (string-match "^['\"]" completion-candidate)))
    (replace-regexp-in-string "\\([ '\"#]\\)" "\\\\\\1" completion-candidate))
   ((eq ?' open-quote)
    (replace-regexp-in-string "'" "'\\''" completion-candidate :literal t))
   ((eq ?\" open-quote)
    (replace-regexp-in-string "\"" "\\\"" completion-candidate :literal t))
   (t
    completion-candidate)))

(defconst bash-completion-known-suffixes-regexp
  (concat "[" (regexp-quote bash-completion-wordbreaks-str) "/ ]$")
  "Regexp matching known suffixes for `bash-completion-addsuffix'.")

(defun bash-completion-addsuffix (str)
  "Add a directory suffix to STR if it looks like a directory.

This function looks for a directory called STR relative to the
buffer-local variable default-directory. If it exists, it returns
\(concat STR \"/\"). Otherwise it retruns STR."
  (if (and (null (string-match bash-completion-known-suffixes-regexp str))
	   (file-accessible-directory-p (expand-file-name str default-directory)))
	(concat str "/")
    str))

(defun bash-completion-before-last-wordbreak (str)
  "Return the part of STR that comes after the last wordbreak character.
The return value does not include the worbreak character itself.

If no wordbreak was found, it returns STR.

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (car (bash-completion-last-wordbreak-split str)))

(defun bash-completion-after-last-wordbreak (str)
  "Return the part of STR that comes before the last wordbreak character.
The return value includes the worbreak character itself.

If no wordbreak was found, it returns \"\".

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (cdr (bash-completion-last-wordbreak-split str)))

(defun bash-completion-last-wordbreak-split (str)
  "Split STR at the last wordbreak character.

The part before the last wordbreak character includes the
wordbreak character itself.  It is \"\" if no wordbreak character
was found.

The part after the last wordbreak character does not include the
wordbreak character.  It is STR if no wordbreak character was
found.

Wordbreaks characters are defined in 'bash-completion-wordbreak'.

Return a CONS containing (before . after)."
  (catch 'bash-completion-return
    (let ((end (- (length str) 1)))
      (while (>= end 0)
	(when (memq (aref str end) bash-completion-wordbreaks)
	  (throw 'bash-completion-return (cons (substring str 0 (1+ end)) (substring str (1+ end)))))
	(setq end (1- end))))
      (cons "" str)))

(defun bash-completion-ends-with (str suffix)
  "Return t if STR ends with SUFFIX."
  (let ((suffix-len (length suffix))
	(str-len (length str)))
    (or
     (= 0 suffix-len)
     (and
      (>= str-len suffix-len)
      (equal (substring str (- suffix-len)) suffix)))))

(defun bash-completion-starts-with (str prefix)
  "Return t if STR starts with PREFIX."
  (let ((prefix-len (length prefix))
	(str-len (length str)))
    (and
     (>= str-len prefix-len)
     (equal (substring str 0 prefix-len) prefix))))

;;; ---------- Functions: bash subprocess

(defun bash-completion-require-process ()
  "Return the bash completion process or start it.

If a bash completion process is already running, return it.

Otherwise, create a bash completion process and return the
result.  This can take a since bash needs to start completely
before this function returns to be sure everything has been
initialized correctly.

The process uses `bash-completion-prog' to figure out the path to
bash on the current system.

To get an environment consistent with shells started with `shell',
the first file found in the following list are sourced if they exist:
 ~/.emacs_bash.sh
 ~/.emacs.d/init_bash.sh
Or, to be more exact, ~/.emacs_$(basename `bash-completion-prog').sh)
and ~/.emacs.d/init_$(basename `bash-completion-prog').sh)

To allow scripts to tell the difference between shells launched
by bash-completion, the environment variable EMACS_BASH_COMPLETE
is set to t."
  (if (bash-completion-is-running)
      bash-completion-process
    ;; start process
    (let ((process) (oldterm (getenv "TERM")))
      (unwind-protect
	  (progn
	    (setenv "EMACS_BASH_COMPLETE" "t")
	    (setenv "TERM" "dumb")
	    (setq process
		  (start-process
		   "*bash-completion*"
		   (generate-new-buffer-name " bash-completion")
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
	    ;; attempt to turn off unexpected status messages from bash
	    ;; if the current version of bash does not support these options,
	    ;; the commands will fail silently and be ignored.
	    (bash-completion-send "shopt -u checkjobs" process)
	    (bash-completion-send "shopt -u mailwarn" process)
	    (bash-completion-send "export MAILCHECK=-1" process)
	    (bash-completion-send "export -n MAIL" process)
	    (bash-completion-send "export -n MAILPATH" process)
	    ;; some bash completion functions use quote_readline to double-quote
	    ;; strings - which compgen understands but only in some environment.
	    ;; disable this dreadful business to get a saner way of handling
	    ;; spaces. Noticed in bash_completion v1.872.
	    (bash-completion-send "function quote_readline { echo \"$1\"; }" process)
	    (bash-completion-send "complete -p" process)
	    (bash-completion-build-alist (process-buffer process))
	    (setq bash-completion-process process)
	    (setq process nil)
	    bash-completion-process)
	;; finally
	(progn
	  (setenv "EMACS_BASH_COMPLETE" nil)
	  (setenv "TERM" oldterm)
	  (when process
	    (condition-case err
		(bash-completion-kill process)
	      (error nil))))))))

(defun bash-completion-cd-command-prefix ()
  "Build a command-line that CD to default-directory.

Return a bash command-line for going to default-directory or \"\"."
  (if default-directory
      (concat "cd >/dev/null 2>&1 "
	      (bash-completion-quote (expand-file-name default-directory))
	      " ; ")
    ""))

(defun bash-completion-build-alist (buffer)
  "Build `bash-completion-alist' with the content of BUFFER.

BUFFER should contains the output of:
  complete -p

Return `bash-completion-alist', which is slightly parsed version
of the output of \"complete -p\"."
  (with-current-buffer buffer
    (save-excursion
      (setq bash-completion-alist nil)
      (goto-char (point-max))
      (while (= 0 (forward-line -1))
	(bash-completion-add-to-alist
	 (bash-completion-strings-from-tokens
	  (bash-completion-tokenize
	   (line-beginning-position)
	   (line-end-position)))))))
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

(defun bash-completion-generate-line (line pos words cword)
  "Generate a command-line that calls compgen.

This function looks into `bash-completion-alist' for a matching compgen
argument set. If it finds one, it executes it. Otherwise, it executes the
default bash completion (compgen -o default)

LINE is the command-line to complete.
POS is the position of the cursor on LINE
WORDS is the content of LINE split by words and unescaped
CWORD is the word 0-based index of the word to complete in WORDS

If the compgen argument set specifies a custom function or command, the
arguments will be passed to this function or command as:
 COMP_LINE, taken from LINE
 COMP_POINT, taken from POS
 COMP_WORDS, taken from WORDS (a bash array)
 COMP_CWORD, taken for CWORD

Return a bash command-line that calls compgen to get the completion
candidates."
  (concat
   (bash-completion-cd-command-prefix)
   (let* ( (command-name (file-name-nondirectory (car words)))
	   (compgen-args (cdr (assoc command-name bash-completion-alist)))
	   (stub (nth cword words)) )
     (cond
      ((= cword 0)
       ;; a command. let emacs expand executable, let bash
       ;; expand builtins, aliases and functions
       (concat "compgen -S ' ' -b -c -a -A function " stub))

      ((not compgen-args)
       ;; no completion configured for this command
       (bash-completion-join (list "compgen" "-o" "default" stub)))

      ((or (member "-F" compgen-args) (member "-C" compgen-args))
       ;; custom completion with a function of command
       (let* ( (args (copy-tree compgen-args))
	       (function (or (member "-F" args) (member "-C" args)))
	       (function-name (car (cdr function))) )
	 (setcar function "-F")
	 (setcar (cdr function) "__bash_complete_wrapper")
	 (format "__BASH_COMPLETE_WRAPPER=%s compgen %s -- %s"
		 (bash-completion-quote
		  (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
			  (bash-completion-quote line)
			  pos
			  cword
			  (bash-completion-join words)
			  (bash-completion-quote function-name)))
		 (bash-completion-join args)
		 (bash-completion-quote stub))))
      (t
       ;; simple custom completion
       (format "compgen %s -- %s" (bash-completion-join compgen-args) stub))))))

;;;###autoload
(defun bash-completion-reset ()
  "Force the next completion command to start with a fresh BASH process.

This function kills any existing BASH completion process.  This way, the
next time BASH completion is requested, a new process will be created with
the latest configuration.

Call this method if you have updated your .bashrc or any bash init scripts
and would like bash completion in Emacs to take these changes into account."
  (interactive)
  (bash-completion-kill bash-completion-process)
  (setq bash-completion-process nil))

(defun bash-completion-kill (process)
  "Kill PROCESS and its buffer."
  (when process
    (when (eq 'run (process-status process))
      (kill-process process))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

(defun bash-completion-buffer ()
  "Return the buffer of the BASH process, create the BASH process if necessary."
  (process-buffer (bash-completion-require-process)))

(defun bash-completion-is-running ()
  "Check whether the bash completion process is running."
  (and bash-completion-process (eq 'run (process-status bash-completion-process))))

(defun bash-completion-send (commandline &optional process timeout)
  "Send a command to the bash completion process.

COMMANDLINE should be a bash command, without the final newline.

PROCESS should be the bash process, if nil this function calls
`bash-completion-require-process' which might start a new process.

TIMEOUT is the timeout value for this operation, if nil the value of
`bash-completion-process-timeout' is used.

Once this command has run without errors, you will find the result
of the command in the bash completion process buffer."
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

(provide 'bash-completion)
;;; bash-completion.el ends here
