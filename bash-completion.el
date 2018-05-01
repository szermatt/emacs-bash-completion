;;; bash-completion.el --- BASH completion for the shell buffer -*- lexical-binding: t -*-

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
;; Bash completion can also be run programatically, outside of a
;; shell-mode command, by calling
;; `bash-completion-dynamic-complete-nocomint'
;;
;; INSTALLATION
;;
;; 1. copy bash-completion.el into a directory that's on Emacs load-path
;; 2. add this into your .emacs file:
;;   (autoload 'bash-completion-dynamic-complete \"bash-completion\"
;;     \"BASH completion hook\")
;;   (add-hook 'shell-dynamic-complete-functions
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
;; bash-completion.el is known to work on Emacs 22 and later under
;; Linux and OSX. It does not works on XEmacs.
;;

;;; History:
;;
;; Full history is available on
;; https://github.com/szermatt/emacs-bash-completion

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

(defcustom bash-completion-args '("--noediting")
  "Args passed to the BASH shell."
  :type '(repeat (string :tag "Argument"))
  :group 'bash-completion)

(defcustom bash-completion-process-timeout 2.5
  "Number of seconds to wait for an answer from bash.
If bash takes longer than that to answer, the answer will be
ignored."
  :type '(float)
  :group 'bash-completion)

(defcustom bash-completion-message-delay 0.4
  "Time to wait before displaying a message while waiting for results.

If completion takes longer than that time, a message is displayed
on the minibuffer to make it clear what's happening. Set to nil
to never display any such message. 0 to always display it.

Only relevant when using bash completion in a shell, through
`bash-completion-dynamic-complete'."
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

(defcustom bash-completion-default-completion t
  "Use Readline’s default filename completion if a compspec
  generates no matches."
  :type 'boolean
  :group 'bash-completion)

(if (fboundp 'completion-table-with-cache)
    (defcustom bash-completion-enable-caching nil
      "If non-nil, enable caching in `bash-completion-dynamic-complete-nocomint'.

When caching is enabled,
`bash-completion-dynamic-complete-nocomint' returns a function
instead of the list of all possible completions. Enabling caching
improves performance because less calls will be made to
`bash-completion-comm' which is an expensive function.

Wordbreak completions behaves slightly differently when this operation is
enabled."
        :type 'boolean
        :group 'bash-completion)
  (defconst bash-completion-enable-caching nil))

(defalias 'bash-completion--completion-table-with-cache
  (if (fboundp 'completion-table-with-cache)
      'completion-table-with-cache 'completion-table-dynamic))

(defvar bash-completion-start-files
  '("~/.emacs_bash.sh" "~/.emacs.d/init_bash.sh")
  "Shell files that, if they exist, will be sourced at the
beginning of a bash completion subprocess.")

;;; ---------- Internal variables and constants

(defvar bash-completion-processes nil
  "Bash processes alist.

Mapping between remote paths as returned by `file-remote-p' and
Bash processes")
(defvar bash-completion-alist nil
  "Maps from command name to the 'complete' arguments.

For example if the following completion is defined in bash:
  complete -F _cdargs_aliases cdb
the following entry is added to `bash-completion-alist':
 (\"cdb\" . (\"-F\" \"_cdargs\"))

See `bash-completion-add-to-alist'.")

(defconst bash-completion-wordbreaks-str "@><=;|&(:"
  "String of word break characters.
This is the equivalent of COMP_WORDBREAKS: special characters
that are considered word breaks in some cases when doing
completion.  This was introduced initially to support file
completion in colon-separated values.")

(defconst bash-completion-wordbreaks
  (append bash-completion-wordbreaks-str nil)
  "`bash-completion-wordbreaks-str' as a list of characters.")

(defconst bash-completion-special-chars "[^-0-9a-zA-Z_./\n=]"
  "Regexp of characters that must be escaped or quoted.")

(defconst bash-completion-wrapped-status
  "\e\ebash-completion-wrapped-status=124\e\e"
  "String output by __bash_complete_wrapper when the wrapped
function returns status code 124, meaning that the completion
should be retried. This should be a string that's unlikely
to be included into a completion output.")

(eval-when-compile
  (unless (or (and (= emacs-major-version 24) (>= emacs-minor-version 1))
              (>= emacs-major-version 25))
    (error
     (concat
      "Emacs version 24.1 or later is required to run emacs-bash-completion.\n"
      "Download emacs-bash-completion version 2.1 to run on older Emacs "
      "versions, from 22 to 24."))))

;;; ---------- Struct

(cl-defstruct (bash-completion-
               (:constructor bash-completion--make)
               (:copier nil))
  line
  point
  words
  cword
  stub-start
  unparsed-stub
  open-quote)

(defsubst bash-completion--stub (comp)
  (nth (bash-completion--cword comp) (bash-completion--words comp)))

;;; ---------- Inline functions

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

;;; ---------- Functions: completion

;;;###autoload
(defun bash-completion-setup ()
  "Register bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions'.

This function is convenient, but it might not be the best way of enabling
bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

  (autoload 'bash-completion-dynamic-complete \"bash-completion\"
    \"BASH completion hook\")
  (add-hook 'shell-dynamic-complete-functions
  	  'bash-completion-dynamic-complete)
"
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete))

;;;###autoload
(defun bash-completion-dynamic-complete ()
    "Return the completion table for bash command at point.

This function is meant to be added into
`shell-dynamic-complete-functions'.  It uses `comint' to figure
out what the current command is and returns a completion table or
nil if no completions available.

When doing completion outside of a comint buffer, call
`bash-completion-dynamic-complete-nocomint' instead."
    (let ((message-timer
           (if (and (not (window-minibuffer-p))
                    (not (null bash-completion-message-delay)))
               (run-at-time
                bash-completion-message-delay nil
                (lambda () (message "Bash completion..."))))))
      (unwind-protect
          (bash-completion-dynamic-complete-nocomint
           (comint-line-beginning-position)
           (point))
        ;; cleanup
        (if message-timer
            (cancel-timer message-timer)))))

;;;###autoload
(defun bash-completion-dynamic-complete-nocomint (comp-start comp-pos)
  "Return completion information for bash command at an arbitrary position.

The bash command to be completed begins at COMP-START in the
current buffer. COMP-POS is the point where completion should
happen.

This function is meant to be usable even in non comint buffers.
It is meant to be called directly from any completion engine.

Returns (list stub-start stub-end completions) with
 - stub-start, the position at which the completed region starts
 - stub-end, the position at which the completed region ends
 - completions, a possibly empty list of completion candidates or a function if
   `bash-completion-enable-caching' is non-nil"
  (when bash-completion-enabled
    (let* ((comp (bash-completion--parse comp-start comp-pos))
	   (line (bash-completion--line comp))
	   (point (bash-completion--point comp))
	   (cword (bash-completion--cword comp))
	   (words (bash-completion--words comp))
           (open-quote (bash-completion--open-quote comp))
	   (stub-start (bash-completion--stub-start comp))
           (stub (bash-completion--stub comp))
           (unparsed-stub (bash-completion--unparsed-stub comp)))
      (if bash-completion-enable-caching
          (list
           stub-start
           comp-pos
           (bash-completion--completion-table-with-cache
            (lambda (_)
              (or (bash-completion-comm comp)
                  (pcase-let ((`(,wordbreak-start _ ,wordbreak-collection)
                               (bash-completion--try-wordbreak-complete
                                stub unparsed-stub stub-start comp-pos
                                open-quote)))
                    (if wordbreak-collection
                        ;; prepend the part of unparsed-stub before
                        ;; the wordbreak.
                        (let ((before-wordbreak
                               (substring unparsed-stub 0
                                          (- wordbreak-start stub-start))))
                          (mapcar (lambda (c) (concat before-wordbreak c))
                                  wordbreak-collection))))))))
        (let ((completions (bash-completion-comm comp)))
          (if completions
              (list stub-start comp-pos completions)
            (bash-completion--try-wordbreak-complete
             stub unparsed-stub stub-start comp-pos open-quote)))))))

(defun bash-completion--try-wordbreak-complete
    (parsed-stub unparsed-stub stub-start pos open-quote)
  "Try wordbreak completion on PARSED-STUB if the complete completion failed.

Split PARSED-STUB using the wordbreak list and apply compgen
default completion on the last part. Return non-nil if a match
was found. The original version of the stub is UNPARSED-STUB. It
can be found on the buffer, between STUB-START and POS.

If PARSED-STUB is quoted, the quote character, ' or \", should be
passed to the parameter OPEN-QUOTE.

This function is not meant to be called outside of
`bash-completion-dynamic-complete'."
  (let* ((wordbreak-split (bash-completion-last-wordbreak-split parsed-stub))
         (before-wordbreak (nth 0 wordbreak-split))
	 (after-wordbreak (nth 1 wordbreak-split))
         (separator (nth 2 wordbreak-split))
         (after-wordbreak-in-unparsed-pos
          (1+ (or (bash-completion--find-last separator unparsed-stub) -1)))
         (unparsed-after-wordbreak
          (substring unparsed-stub
                     after-wordbreak-in-unparsed-pos
                     (length unparsed-stub))))
    (when (> (length before-wordbreak) 0)
      (list (+ stub-start after-wordbreak-in-unparsed-pos)
            pos
            (bash-completion--default-completion
             after-wordbreak unparsed-after-wordbreak
             open-quote)))))

(defun bash-completion--find-last (elt array)
  "Return the position of the last intance of ELT in array or nil."
  (catch 'bash-completion-return
    (let ((array-len (length array)))
      (dotimes (index array-len)
        (if (eq elt (aref array (- array-len index 1)))
            (throw 'bash-completion-return (- array-len index 1)))))
    nil))

(defun bash-completion--default-completion
    (stub unparsed-stub open-quote)
  "Do default completion on the given STUB.

Return the extracted candidate, with STUB replaced with
UNPARSED-STUB, taking OPEN-QUOTE into account."
  (when (eq 0 (bash-completion-send (concat
                                     (bash-completion-cd-command-prefix)
                                     "compgen -o default -- "
                                     (bash-completion-quote stub))))
    (bash-completion-extract-candidates
     stub unparsed-stub open-quote
     'default)))

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
	    (replace-regexp-in-string "'" "'\\''" word nil t)
	    "'")))

(defun bash-completion--parse (comp-start comp-pos)
  "Process a command line split into TOKENS that end at POS.

If stub is quoted, the quote character should be passed as
OPEN-QUOTE.

This function takes a list of tokens built by
`bash-completion-tokenize' and returns the variables compgen
function expect in an association list.

Return an association list with the current symbol as keys:
 line - the relevant command between START and POS (string)
 point - 0-based position of the cursor in line (number)
 cword - 0-based index of the word to be completed in words (number)
 words - line split into words, unescaped (list of strings)
 stub-start - start position of the thing we are completing
 unparsed-stub - unparsed version of (nth cword words)
 open-quote - quote open at stub end: nil, ?' or ?\""
  (let* ((all-tokens (bash-completion-tokenize comp-start comp-pos))
         (line-tokens (bash-completion-parse-current-command  all-tokens))
         (first-token (car line-tokens))
	 (last-token (car (last line-tokens)))
         (open-quote (bash-completion-tokenize-open-quote line-tokens))
	 (start (or (car (bash-completion-tokenize-get-range first-token)) comp-pos))
	 (end (or (cdr (bash-completion-tokenize-get-range last-token)) comp-pos))
	 (words (bash-completion-strings-from-tokens line-tokens))
	 (stub-empty (or (> comp-pos end) (= start end)))
	 (stub-start
	  (if stub-empty
	      comp-pos
	    (+ (car (bash-completion-tokenize-get-range last-token))
	       (if open-quote 1 0)))))
    (when stub-empty (setq words (append words '(""))))
    (bash-completion--make
     :line (buffer-substring-no-properties start comp-pos)
     :point (- comp-pos start)
     :cword (- (length words) 1)
     :words words
     :stub-start stub-start
     :unparsed-stub (buffer-substring-no-properties stub-start comp-pos)
     :open-quote open-quote)))

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
	 (let* ((string (bash-completion-tokenize-get-str token))
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

This function takes all strings from TOKENS and return it as a
list of strings.

TOKENS should be in the format returned by `bash-completion-tokenize'."
  (mapcar 'bash-completion-tokenize-get-str tokens))

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
  (let ((char-start (char-after))
	(quote nil) )
    (when (and char-start (or (= char-start ?') (= char-start ?\")))
      (forward-char)
      (setq quote char-start))
    (bash-completion-tokenize-1 end quote tokens token)))

(defun bash-completion-tokenize-1 (end quote tokens token)
  "Tokenize the rest of the token.

This function is meant to be called exclusively from
`bash-completion-tokenize-0'.

This function tokenizes the rest of the token and either calls
itself and `bash-completion-tokenize-0' recursively or appends
the token to the list of token and calls
`bash-completion-tokenize-new-element' to look for the next
token.

END specifies the point at which tokenization should stop.

QUOTE specifies the current quote.  It should be nil, ?' or ?\"

TOKENS is the list of tokens built so far in reverse order.

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
   ;; inside a quote
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

(defun bash-completion-comm (comp)
  "Call compgen on COMP return the result.

COMP should be a struct returned by `bash-completion--parse'

This function starts a separate bash process if necessary, sets
up the completion environment (COMP_LINE, COMP_POINT, COMP_WORDS,
COMP_CWORD) and calls compgen.

The result is a list of candidates, which might be empty."
  ;; start process now, to make sure bash-completion-alist is
  ;; set before we run bash-completion-generate-line
  
  (let* ((entry (bash-completion-require-process))
         (process (car entry))
         (bash-completion-alist (cdr entry))
         (cmdline)
         (candidates)
         (completion-status))
    (setq cmdline (bash-completion-generate-line comp t))
    (setq completion-status (bash-completion-send (cdr cmdline) process))
    (when (eq 124 completion-status)
      ;; Special 'retry-completion' exit status, typically returned by
      ;; functions bound by complete -D. Presumably, the function has
      ;; just setup completion for the current command and is asking
      ;; us to retry once with the new configuration. 
      (bash-completion-send "complete -p" process)
      (bash-completion-build-alist (process-buffer process))
      (setcdr entry bash-completion-alist)
      (setq cmdline (bash-completion-generate-line comp nil))
      (setq completion-status (bash-completion-send (cdr cmdline) process)))
    (setq candidates
          (when (eq 0 completion-status)
            (bash-completion-extract-candidates
             (bash-completion--stub comp)
             (bash-completion--unparsed-stub comp)
             (bash-completion--open-quote comp)
             (car cmdline))))
    (if (and bash-completion-default-completion (not candidates) (eq 'custom (car cmdline)))
        (bash-completion--default-completion
         (bash-completion--stub comp)
         (bash-completion--unparsed-stub comp)
         (bash-completion--open-quote comp))
      candidates)))

(defun bash-completion-extract-candidates
    (parsed-stub unparsed-stub open-quote completion-type)
  "Extract the completion candidates from the process buffer for PARSED-STUB.

This command takes the content of the completion process buffer,
splits it by newlines, post-process the candidates and returns
them as a list of strings.

It should be invoked with the comint buffer as the current buffer
for directory name detection to work.

If PARSED-STUB is quoted, the quote character, ' or \", should be
passed in OPEN-QUOTE.

If IS-COMMAND is t, it is passed down to `bash-completion-suffix'

Post-processing includes escaping special characters, adding a /
to directory names, replacing STUB with UNPARSED-STUB in the
result. See `bash-completion-fix' for more details."
  (let ((candidates) (result (list)))
    (setq candidates (with-current-buffer (bash-completion-buffer)
                       (split-string (buffer-string) "\n" t)))
    (if (eq 1 (length candidates))
        (list (bash-completion-fix
               (car candidates) parsed-stub unparsed-stub
               open-quote completion-type t))
      (dolist (completion candidates)
        (push (bash-completion-fix
               completion parsed-stub unparsed-stub open-quote completion-type nil)
              result))
      (delete-dups (nreverse result)))))

(defun bash-completion-fix
    (str parsed-prefix unparsed-prefix open-quote completion-type single)
  "Fix completion candidate in STR if PREFIX is the current prefix.

STR is the completion candidate to modify.

PARSED-PREFIX should be the current string being completed. If it
is nil, the value of `bash-completion-prefix' is used. This
allows calling this function from `mapcar'.

PARSED-PREFIX is replaced with UNPARSED-PREFIX in set fixed set
of candidates.

OPEN-QUOTE should be the quote that's still open in prefix.  A
character (' or \") or nil.  

COMPLETION-TYPE describes the type of completion that was
executed: 'default, 'custom or 'command. It is used
to choose whether to add a space and detect directories.

If SINGLE is non-nil, this is the single completion candidate.

Return a modified version of the completion candidate.

Modification include:
 - escaping of special characters in STR
 - prepending PREFIX if STR does not contain all of it, when
   completion was done after a wordbreak
 - adding / to recognized directory names

It should be invoked with the comint buffer as the current buffer
for directory name detection to work."
  (let ((suffix "")
        (rest) ; the part between the prefix and the suffix
        (rebuilt))

    ;; build rest by removing parsed-prefix from str
    (cond
     ((bash-completion-starts-with str parsed-prefix)
      (setq rest (substring str (length parsed-prefix))))

     ;; unexpand the home directory expanded by bash automatically
     ((and (bash-completion-starts-with parsed-prefix "~")
           (bash-completion-starts-with str (bash-completion--expand-file-name "~" t)))
      (setq rest (substring (concat "~" (substring str (length (bash-completion--expand-file-name "~" t))))
                            (length parsed-prefix))))

     ((bash-completion-starts-with parsed-prefix str)
      ;; completion is a substring of prefix something's gone
      ;; wrong. Treat it as one (useless) candidate.
      (setq unparsed-prefix "")
      (setq rest str))

     ;; completion sometimes only applies to the last word, as
     ;; defined by COMP_WORDBREAKS. This detects and works around
     ;; this feature.
     ((bash-completion-starts-with
       (setq rebuilt (concat (bash-completion-before-last-wordbreak parsed-prefix) str))
       parsed-prefix)
      (setq rest (substring rebuilt (length parsed-prefix))))

     ;; there is no meaningful link between the prefix and
     ;; the string. just append the string to the prefix and
     ;; hope for the best.
     (t (setq rest str)))

    ;; build suffix
    (let ((last-char (bash-completion-last-char rest))
          (close-quote-str (if open-quote (char-to-string open-quote) ""))
          (final-space-str (if bash-completion-nospace "" " ")))
      (cond
       ((eq ?\  last-char)
        (setq rest (substring rest 0 -1))
        (setq suffix (concat close-quote-str final-space-str)))
       ((or (memq last-char bash-completion-wordbreaks)
            (eq ?/ last-char))
        (setq suffix ""))
       ((and
         (memq completion-type '(command default custom))
         (file-accessible-directory-p
          (bash-completion--expand-file-name (bash-completion-unescape
                                              open-quote (concat parsed-prefix rest)))))
        (setq suffix "/"))
       ((or (eq completion-type 'command)
            (and (memq completion-type '(default custom))
                 single))
        (setq suffix (concat close-quote-str final-space-str)))
       (t (setq suffix close-quote-str))))

    ;; put everything back together
    (concat unparsed-prefix
            (bash-completion-escape-candidate rest open-quote)
            suffix)))

(defun bash-completion-escape-candidate (completion-candidate open-quote)
  "Escapes COMPLETION-CANDIDATE.

This function escapes all special characters in the result of
bash completion.  It does nothing if COMPLETION-CANDIDATE looks
like a quoted string.

It uses escape characters appropriate for the quote defined in
OPEN-QUOTE, either nil, ' or \".

Return a possibly escaped version of COMPLETION-CANDIDATE."
  (cond
   ((zerop (length completion-candidate)) "")
   ((null open-quote)
    (replace-regexp-in-string
     "\n" "'\n'"
     (replace-regexp-in-string
      bash-completion-special-chars "\\\\\\&" completion-candidate)))
   ((eq ?' open-quote)
    (replace-regexp-in-string "'" "'\\''" completion-candidate nil t))
   ((eq ?\" open-quote)
    ;; quote '$', '`' or '"'
    (replace-regexp-in-string
     "[$`\"]" "\\\\\\&"
     ;; quote backslash if it's followed by '$', '`' or '"'
     (replace-regexp-in-string "\\\\\\([$`\"]\\)" "\\\\\\\\\\1" completion-candidate)))
   (t
    completion-candidate)))

(defun bash-completion-unescape (open-quote string)
  "Unescapes a possibly QUOTE'ed STRING."
  (if (eq ?' open-quote)
      (replace-regexp-in-string "'\\\\''" "'" string)
    (replace-regexp-in-string "\\(\\\\\\)\\(.\\)" "\\2" string)))

(defun bash-completion-before-last-wordbreak (str)
  "Return the part of STR that comes after the last wordbreak character.
The return value does not include the worbreak character itself.

If no wordbreak was found, it returns STR.

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (nth 0 (bash-completion-last-wordbreak-split str)))

(defun bash-completion-after-last-wordbreak (str)
  "Return the part of STR that comes before the last wordbreak character.
The return value includes the worbreak character itself.

If no wordbreak was found, it returns \"\".

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (nth 1 (bash-completion-last-wordbreak-split str)))

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
    (let ((end (- (length str) 1))
          (breakc))
      (while (>= end 0)
        (setq breakc (memq (aref str end) bash-completion-wordbreaks))
	(when breakc
	  (throw 'bash-completion-return
                 (list (substring str 0 (1+ end))
                       (substring str (1+ end))
                       (car breakc))))
	(setq end (1- end))))
      (list "" str ?\0)))

(defun bash-completion-last-char (str)
  "Returns the last char of STR or nil."
  (let ((str-len (length str)))
    (and (>= str-len 1)
         (aref str (1- str-len)))))

(defun bash-completion-starts-with (str prefix)
  "Return t if STR starts with PREFIX."
  (let ((prefix-len (length prefix))
	(str-len (length str)))
    (and
     (>= str-len prefix-len)
     (string= (substring str 0 prefix-len) prefix))))

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
  (let ((remote (file-remote-p default-directory)))
    (if (bash-completion-is-running)
        (cdr (assoc remote bash-completion-processes))
      ;; start process
      (let ((process) (oldterm (getenv "TERM")) (cleanup t))
        (unwind-protect
            (progn
              (setenv "EMACS_BASH_COMPLETE" "t")
              (setenv "TERM" "dumb")
              (let* ((start-proc-fun (if remote #'start-file-process #'start-process))
                     (buffer-name (generate-new-buffer-name " bash-completion"))
                     (args `("*bash-completion*"
                             ,buffer-name
                             ,bash-completion-prog
                             ,@bash-completion-args)))
                (when remote
                  ;; See http://lists.gnu.org/archive/html/tramp-devel/2016-05/msg00004.html
                  (get-buffer-create buffer-name))
                (let ((non-essential (if remote nil non-essential)))
                  ;; Set `non-essential' to nil when spawning a remote
                  ;; shell to ensure that Tramp will try to open a
                  ;; connection to the remote host. Otherwise the
                  ;; process will be launched on the localhost. This
                  ;; is need because some completion framework (e.g
                  ;; company) set `non-essential' to a non-nil value
                  ;; when the completion has not been requested by the
                  ;; user
                  (setq process (apply start-proc-fun args))))
              (set-process-query-on-exit-flag process nil)
              (dolist (start-file bash-completion-start-files)
                (when (file-exists-p (bash-completion--expand-file-name start-file))
                  (process-send-string process (concat ". " start-file "\n"))))
              (bash-completion-send "PROMPT_COMMAND='';PS1='\t$?\v'" process bash-completion-initial-timeout)
              (bash-completion-send (concat "function __bash_complete_wrapper {"
                                            " eval $__BASH_COMPLETE_WRAPPER;"
                                            " n=$?; if [[ $n = 124 ]]; then"
                                            "  echo -n \""
                                            bash-completion-wrapped-status
                                            "\"; return 1; "
                                            " fi; }") process)
              ;; attempt to turn off unexpected status messages from bash
              ;; if the current version of bash does not support these options,
              ;; the commands will fail silently and be ignored.
              (bash-completion-send "shopt -u checkjobs" process)
              (bash-completion-send "shopt -u mailwarn" process)
              (bash-completion-send "export MAILCHECK=-1" process)
              (bash-completion-send "export -n MAIL" process)
              (bash-completion-send "export -n MAILPATH" process)
              (bash-completion-send "unset HISTFILE" process)
              ;; some bash completion functions use quote_readline to double-quote
              ;; strings - which compgen understands but only in some environment.
              ;; disable this dreadful business to get a saner way of handling
              ;; spaces. Noticed in bash_completion v1.872.
              (bash-completion-send "function quote_readline { echo \"$1\"; }" process)
              (bash-completion-send "complete -p" process)
              (bash-completion-build-alist (process-buffer process))
              (let ((entry (cons process bash-completion-alist)))
                (push (cons remote entry)
                      bash-completion-processes)
                (setq cleanup nil)
                entry))
          ;; finally
          (progn
            (setenv "EMACS_BASH_COMPLETE" nil)
            (setenv "TERM" oldterm)
            (when cleanup
              (condition-case nil
                  (bash-completion-kill process)
                (error nil)))))))))

(defun bash-completion-cd-command-prefix ()
  "Build a command-line that CD to default-directory.

Return a bash command-line for going to default-directory or \"\"."
  (let ((dir (or (file-remote-p (or default-directory "") 'localname)
                 default-directory)))
    (if dir
        (concat "cd >/dev/null 2>&1 "
                (bash-completion-quote (bash-completion--expand-file-name dir t))
                " ; ")
      "")))

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
    (if (member "-D" (cdr words))
	;; default completion 
	(push (cons nil (delete "-D" (cdr words))) bash-completion-alist)
      ;; normal completion
      (let* ((reverse-wordsrest (nreverse (cdr words)))
	     (command (car reverse-wordsrest))
	     (options (nreverse (cdr reverse-wordsrest))) )
	(when (and command options)
	  (push (cons command options) bash-completion-alist)))))
  bash-completion-alist)

(defun bash-completion-generate-line (comp allowdefault)
  "Generate a command-line that calls compgen.

This function looks into `bash-completion-alist' for a matching compgen
argument set. If it finds one, it executes it. Otherwise, it executes the
default bash completion (compgen -o default)

COMP is a struct returned by `bash-completion--parse'
ALLOWDEFAULT controls whether to fallback on a possible -D completion 

If the compgen argument set specifies a custom function or command, the
arguments will be passed to this function or command as:
 COMP_LINE, taken from (bash-completion--line COMP)
 COMP_POINT, taken from (bash-completion--point COMP)
 COMP_WORDS, taken from (bash-completion--words COMP) (a bash array)
 COMP_CWORD, taken for (bash-completion--cword COMP)

Return a cons containing the completion type (command default or
custom) and a bash command-line that calls compgen to get the
completion candidates."
  (let* ( (command-name (file-name-nondirectory (car (bash-completion--words comp))))
          (compgen-args
           (or (cdr (assoc command-name bash-completion-alist))
               (and allowdefault (cdr (assoc nil bash-completion-alist)))))
          (quoted-stub (bash-completion-quote (bash-completion--stub comp)))
          (completion-type)
          (commandline) )
    (cond
      ((= (bash-completion--cword comp) 0)
       ;; a command. let bash expand builtins, aliases and functions
       (setq completion-type 'command)
       (setq commandline (concat "compgen -b -c -a -A function -- " quoted-stub)))

      ((not compgen-args)
       ;; no completion configured for this command
       (setq completion-type 'default)
       (setq commandline (concat "compgen -o default -- " quoted-stub)))

      ((or (member "-F" compgen-args) (member "-C" compgen-args))
       ;; custom completion with a function of command
       (let* ((args (copy-tree compgen-args))
	      (function (or (member "-F" args) (member "-C" args)))
	      (function-name (car (cdr function))) )
	 (setcar function "-F")
	 (setcar (cdr function) "__bash_complete_wrapper")
         (setq completion-type 'custom)
	 (setq commandline
               (format "__BASH_COMPLETE_WRAPPER=%s compgen %s -- %s"
		 (bash-completion-quote
		  (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
			  (bash-completion-quote (bash-completion--line comp))
			  (bash-completion--point comp)
			  (bash-completion--cword comp)
			  (bash-completion-join (bash-completion--words comp))
			  (bash-completion-quote function-name)))
		 (bash-completion-join args)
		 quoted-stub))))
      (t
       ;; simple custom completion
       (setq completion-type 'custom)
       (setq commandline (format "compgen %s -- %s" (bash-completion-join compgen-args)
                                 quoted-stub))))
    (cons completion-type
          (concat
           (bash-completion-cd-command-prefix)
           commandline
           " 2>/dev/null"))))

;;;###autoload
(defun bash-completion-reset ()
  "Force the next completion command to start with a fresh BASH process.

This function kills any existing BASH completion process. This
way, the next time BASH completion is requested, a new process
will be created with the latest configuration. The BASH
completion process that will be killed depends on the
default-directory of the buffer where the command is executed.

Call this method if you have updated your .bashrc or any bash init scripts
and would like bash completion in Emacs to take these changes into account."
  (interactive)
  (let* ((remote (and default-directory (file-remote-p default-directory)))
         (entry (assoc remote bash-completion-processes))
         (proc (cadr entry)))
    (when proc
      (bash-completion-kill proc)
      (setq bash-completion-processes (delq entry bash-completion-processes)))))

(defun bash-completion-reset-all ()
  (interactive)
  (mapcar (lambda (entry)
            (let ((default-directory (car entry)))
              (bash-completion-reset)))
          bash-completion-processes))

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
  (process-buffer (car (bash-completion-require-process))))

(defun bash-completion-is-running ()
  "Check whether the bash completion process is running."
  (let* ((entry (assoc (file-remote-p default-directory)
                       bash-completion-processes))
         (proc (cadr entry))
         (running (and proc (eq 'run (process-status proc)))))
    (unless (and entry running)
      (setq bash-completion-processes (delq entry bash-completion-processes)))
    running))

(defun bash-completion-send (commandline &optional process timeout)
  "Send a command to the bash completion process.

COMMANDLINE should be a bash command, without the final newline.

PROCESS should be the bash process, if nil this function calls
`bash-completion-require-process' which might start a new process.

TIMEOUT is the timeout value for this operation, if nil the value of
`bash-completion-process-timeout' is used.

Once this command has run without errors, you will find the result
of the command in the bash completion process buffer.

Return the status code of the command, as a number."
  ;; (message commandline)
  (let ((process (or process (car (bash-completion-require-process))))
	(timeout (or timeout bash-completion-process-timeout)))
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (process-send-string process (concat commandline "\n"))
      (while (not (progn (goto-char 1) (search-forward "\v" nil t)))
	(unless (accept-process-output process timeout)
	  (error "Timeout while waiting for an answer from bash-completion process")))
      (let* ((control-v-position (point))
	     (control-t-position (progn (search-backward "\t" nil t) (point)))
	     (status-code (string-to-number
			   (buffer-substring-no-properties
			    (1+ control-t-position) (1- control-v-position)))))
	(delete-region control-t-position (point-max))
	(goto-char (point-min))
	(let ((case-fold-search nil))
	  (when (search-forward bash-completion-wrapped-status nil t)
	    (setq status-code 124)
	    (delete-region (match-beginning 0) (match-end 0))))
	;; (message "status: %d content: \"%s\""
	;; 	 status-code
	;; 	 (buffer-substring-no-properties
	;; 	  (point-min) (point-max)))
	status-code))))

(defun bash-completion--expand-file-name (name &optional local-part-only)
  (let* ((remote (file-remote-p default-directory))
         (expanded (if (and remote
                            (not (file-remote-p name))
                            (file-name-absolute-p name))
                       (expand-file-name (concat remote name))
                     (expand-file-name name))))
    (if (and remote local-part-only)
        (file-remote-p expanded 'localname)
      expanded)))

(provide 'bash-completion)
;;; bash-completion.el ends here
