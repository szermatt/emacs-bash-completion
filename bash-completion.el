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
;; bash-completion.el is known to work with Bash 3 and 4, on Emacs,
;; starting with version 24.1, under Linux and OSX. It does not work
;; on XEmacs.
;;

;;; History:
;;
;; Full history is available on
;; https://github.com/szermatt/emacs-bash-completion

(require 'comint)
(eval-when-compile (require 'cl))

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

(defcustom bash-completion-prog (executable-find "bash")
  "Name or path of the BASH executable to run for command-line completion.
This should be either an absolute path to the BASH executable or
the name of the bash command if it is on Emacs' PATH. This should
point to a recent version of BASH, 3 or 4, with support for
command-line completion."
  :type '(file :must-match t)
  :group 'bash-completion)

(defcustom bash-completion-remote-prog "bash"
  "Name or path of the remote BASH executable to use.

This is the path of an BASH executable available on the remote machine.
Best is to just specify \"bash\" and rely on the PATH being set correctly
for the remote connection."
  :type '(string)
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

(defvar bash-completion-start-files
  '("~/.emacs_bash.sh" "~/.emacs.d/init_bash.sh")
  "Shell files that, if they exist, will be sourced at the
beginning of a bash completion subprocess.")

(defvar bash-completion-wordbreaks ""
  "Extra wordbreaks to use when tokenizing, in `bash-completion-tokenize'")

;;; ---------- Internal variables and constants

(defvar bash-completion-processes nil
  "Bash processes alist.

Mapping between remote paths as returned by `file-remote-p' and
Bash processes")

(defconst bash-completion-special-chars "[^-0-9a-zA-Z_./\n=]"
  "Regexp of characters that must be escaped or quoted.")

(eval-when-compile
  (unless (or (and (= emacs-major-version 24) (>= emacs-minor-version 1))
              (>= emacs-major-version 25))
    (error
     (concat
      "Emacs version 24.1 or later is required to run emacs-bash-completion.\n"
      "Download emacs-bash-completion version 2.1 to run on older Emacs "
      "versions, from 22 to 24."))))

;;; ---------- Struct

;; The main, completion structure, keeping track of the definition and
;; state of the current completion.
(defstruct (completion (:constructor bash-completion--make)
                       (:conc-name bash-completion--)
                       (:copier nil))
  line           ; the relevant command (string)
  point          ; 0-based position of the cursor in line (number)
  words          ; line split into words, unescaped (list of strings)
  cword          ; 0-based index of the word to be completed in words (number)
  unparsed-stub  ; unparsed version of the thing we are completing,
                 ; that is, the part of the last word after the last
                 ; wordbreak separator.
  stub-start     ; start position of the thing we are completing
  stub           ; parsed version of the stub
  open-quote     ; quote open at stub end: nil, ?' or ?\""
  compgen-args   ; compgen arguments for this command (list of strings)
  wordbreaks     ; value of COMP_WORDBREAKS active for this completion
  compopt        ; options forced with compopt nil or `(nospace . ,bool) 
)

(defun bash-completion--type (comp)
  "Returns the type of COMP.

Completion type is 'command, if completing a command (cword = 0),
'custom if there's a custom completion for the current command or
'default if there isn't or if the completion hasn't been
customized, usually by `bash-completion--customize'.
"
  (cond
   ((zerop (bash-completion--cword comp)) 'command)
   ((bash-completion--compgen-args comp) 'custom)
   (t 'default)))

(defun bash-completion--nospace (comp)
  "Returns the value of the nospace option to use for COMP.

The option can be:
 - set globally, by setting `bash-completion-nospace' to t
 - set for a customized completion, in bash, with
   '-o' 'nospace'."
  (let ((cell))
    (cond
     (bash-completion-nospace t) ; set globally
     ((setq cell (assq 'nospace (bash-completion--compopt comp)))
      (cdr cell))
     (t (bash-completion--has-compgen-option
         (bash-completion--compgen-args comp) "nospace")))))

(defun bash-completion--command (comp)
  "Return the current command for the completion, if there is one."
  (file-name-nondirectory (car (bash-completion--words comp))))

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
           (point)
           'dynamic-table)
        ;; cleanup
        (if message-timer
            (cancel-timer message-timer)))))

(defalias 'bash-completion--completion-table-with-cache
  (if (fboundp 'completion-table-with-cache)
      'completion-table-with-cache 'completion-table-dynamic))

;;;###autoload
(defun bash-completion-dynamic-complete-nocomint
    (comp-start comp-pos &optional dynamic-table)
  "Return completion information for bash command at an arbitrary position.

The bash command to be completed begins at COMP-START in the
current buffer. COMP-POS is the point where completion should
happen.

This function is meant to be usable even in non comint buffers.
It is meant to be called directly from any completion engine.

Returns (list stub-start stub-end completions) with
 - stub-start, the position at which the completed region starts
 - stub-end, the position at which the completed region ends
 - completions, a possibly empty list of completion candidates
   or a function, if DYNAMIC-TABLE is non-nil, a lambda such as the one
   returned by `completion-table-dynamic'"
  (when bash-completion-enabled
    (let* ((process (bash-completion-require-process))
           (comp (bash-completion--parse
                  comp-start comp-pos
                  (process-get process 'wordbreaks)
                  (process-get process 'bash-major-version)))
	   (stub-start (bash-completion--stub-start comp)))
      (bash-completion--customize comp process)
      (list
       stub-start
       comp-pos
       (if dynamic-table
           (bash-completion--completion-table-with-cache
            (lambda (_)
              (bash-completion-comm comp process)))
         (bash-completion-comm comp process))))))

(defun bash-completion--find-last (elt array)
  "Return the position of the last intance of ELT in array or nil."
  (catch 'bash-completion-return
    (let ((array-len (length array)))
      (dotimes (index array-len)
        (if (eq elt (aref array (- array-len index 1)))
            (throw 'bash-completion-return (- array-len index 1)))))
    nil))

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

(defun bash-completion--parse (comp-start comp-pos wordbreaks bash-major-version)
  "Process a command line split into TOKENS that end at POS.

WORDBREAK is the value of COMP_WORDBREAKS to use for this completion,
usually taken from the current process.

This function takes a list of tokens built by
`bash-completion-tokenize' and returns the variables compgen
function expect in an association list.

Returns a completion struct."
  (let* ((all-tokens (bash-completion-tokenize
                      comp-start comp-pos (if (>= bash-major-version 4)
                                              wordbreaks "")))
         (line-tokens (bash-completion-parse-current-command  all-tokens))
         (first-token (car line-tokens))
	 (last-token (car (last line-tokens)))
         (open-quote (bash-completion-tokenize-open-quote line-tokens))
	 (start (or (car (bash-completion-tokenize-get-range first-token)) comp-pos))
	 (end (or (cdr (bash-completion-tokenize-get-range last-token)) comp-pos))
	 (words (bash-completion-strings-from-tokens line-tokens))
         (stub-start) (unparsed-stub) (parsed-stub))
    (if (or (> comp-pos end) (= start end))
        (setq stub-start comp-pos
              unparsed-stub ""
              parsed-stub ""
              words (append words '("")))
      (if (< bash-major-version 4)
          (setq last-token (car (last (bash-completion-tokenize
                                       comp-start comp-pos wordbreaks)))))
      (setq stub-start (car (bash-completion-tokenize-get-range last-token))
            parsed-stub (bash-completion-tokenize-get-str last-token)
            unparsed-stub (buffer-substring-no-properties stub-start comp-pos)))
    (bash-completion--make
     :line (buffer-substring-no-properties start comp-pos)
     :point (- comp-pos start)
     :cword (- (length words) 1)
     :words words
     :stub-start stub-start
     :unparsed-stub unparsed-stub
     :stub parsed-stub
     :open-quote open-quote
     :wordbreaks wordbreaks)))

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

(defun bash-completion-tokenize (start end &optional wordbreaks)
  "Tokenize the portion of the current buffer between START and END.

This function splits a BASH command line into tokens.  It knows
about quotes, escape characters and special command separators such
as ;, | and &&. If specified WORDBREAKS contains extra word breaks,
usually taken from COMP_WORDBREAKS, to apply while tokenizing.

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
  (let ((bash-completion-wordbreaks
         (mapconcat 'char-to-string
                    (delq nil (mapcar
                               (lambda (c)
                                 (if (memq c '(?\; ?& ?| ?' ?\")) nil c))
                               (or wordbreaks "")))
                    "")))
    (save-excursion
      (goto-char start)
      (nreverse (bash-completion-tokenize-new-element end nil)))))

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
    (when (= (skip-chars-forward
              (concat "[;&|" bash-completion-wordbreaks "]")
              end)
             0)
      (skip-chars-forward
       (bash-completion-nonsep quote bash-completion-wordbreaks) end))
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

(defun bash-completion-nonsep (quote wordbreaks)
  "Return the set of non-breaking characters when QUOTE is the current quote.

QUOTE should be nil, ?' or ?\"."
  (concat
   "^ \t\n\r"
   (if (null quote) (concat ";&|'\"" wordbreaks)
     (char-to-string quote))))

;;; ---------- Functions: getting candidates from bash

(defun bash-completion-comm (comp process)
  "Call compgen on COMP for PROCESS, return the result.

COMP should be a struct returned by `bash-completion--parse'

This function starts a separate bash process if necessary, sets
up the completion environment (COMP_LINE, COMP_POINT, COMP_WORDS,
COMP_CWORD) and calls compgen.

The result is a list of candidates, which might be empty."
  (let* ((buffer (process-buffer process))
         (completion-status))
    (setq completion-status (bash-completion-send (bash-completion-generate-line comp) process))
    (when (eq 124 completion-status)
      ;; Special 'retry-completion' exit status, typically returned by
      ;; functions bound by complete -D. Presumably, the function has
      ;; just setup completion for the current command and is asking
      ;; us to retry once with the new configuration.
      (bash-completion-send "complete -p" process)
      (process-put process 'complete-p (bash-completion-build-alist buffer))
      (bash-completion--customize comp process 'nodefault)
      (setq completion-status (bash-completion-send (bash-completion-generate-line comp) process)))
    (when (eq 0 completion-status)
      (bash-completion-extract-candidates comp buffer))))

(defun bash-completion-extract-candidates (comp buffer)
  "Extract the completion candidates for COMP form BUFFER.

This command takes the content of the completion process buffer,
splits it by newlines, post-process the candidates and returns
them as a list of strings.

It should be invoked with the comint buffer as the current buffer
for directory name detection to work.

Post-processing includes escaping special characters, adding a /
to directory names, replacing STUB with UNPARSED-STUB in the
result. See `bash-completion-fix' for more details."
  (let ((output) (candidates))
    (with-current-buffer buffer
      (let ((compopt (bash-completion--parse-side-channel-data "compopt")))
        (cond
         ((string= "-o nospace" compopt)
          (setf (bash-completion--compopt comp) '((nospace . t))))
         ((string= "+o nospace" compopt)
          (setf (bash-completion--compopt comp) '((nospace . nil))))))
      (setq output (buffer-string)))
    (setq candidates (delete-dups (split-string output "\n" t)))
    (if (eq 1 (length candidates))
        (list (bash-completion-fix (car candidates) comp t))
      ;; multiple candidates
      (let ((result (list)))
        (dolist (completion candidates)
          (push (bash-completion-fix completion comp nil) result))
        (delete-dups (nreverse result))))))

(defun bash-completion-fix (str comp single)
  "Fix completion candidate in STR for COMP

STR is the completion candidate to modify, COMP the current
completion operation.

If STR is the single candidate, SINGLE is t.

Return a modified version of STR.

Modification include:
 - escaping of special characters in STR
 - prepending the stub if STR does not contain all of it, when
   completion was done after a wordbreak
 - adding / to recognized directory names

It should be invoked with the comint buffer as the current buffer
for directory name detection to work."
  (let ((parsed-prefix (bash-completion--stub comp))
        (unparsed-prefix (bash-completion--unparsed-stub comp))
        (open-quote (bash-completion--open-quote comp))
        (nospace (bash-completion--nospace comp))
        (wordbreaks (bash-completion--wordbreaks comp))
        (suffix "")
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
       (setq rebuilt (concat (bash-completion-before-last-wordbreak parsed-prefix wordbreaks) str))
       parsed-prefix)
      (setq rest (substring rebuilt (length parsed-prefix))))

     ;; there is no meaningful link between the prefix and
     ;; the string. just append the string to the prefix and
     ;; hope for the best.
     (t (setq rest str)))

    ;; build suffix
    (let ((last-char (bash-completion-last-char rest))
          (close-quote-str (if open-quote (char-to-string open-quote) ""))
          (final-space-str (if nospace "" " ")))
      (cond
       ((eq ?\  last-char)
        (setq rest (substring rest 0 -1))
        (setq suffix (concat close-quote-str final-space-str)))
       ((or (bash-completion--find-last last-char wordbreaks)
            (eq ?/ last-char))
        (setq suffix ""))
       ((file-accessible-directory-p
         (bash-completion--expand-file-name (bash-completion-unescape
                                             open-quote (concat parsed-prefix rest))))
        (setq suffix "/"))
       (single
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

(defun bash-completion-before-last-wordbreak (str wordbreaks)
  "Return the part of STR that comes after the last WORDBREAKS character.
The return value does not include the worbreak character itself.

If no wordbreak was found, it returns STR."
  (nth 0 (bash-completion-last-wordbreak-split str wordbreaks)))

(defun bash-completion-last-wordbreak-split (str wordbreaks)
  "Split STR at the last WORDBREAKS character.

The part before the last wordbreak character includes the
wordbreak character itself.  It is \"\" if no wordbreak character
was found.

The part after the last wordbreak character does not include the
wordbreak character.  It is STR if no wordbreak character was
found.

Return a CONS containing (before . after)."
  (catch 'bash-completion-return
    (let ((end (- (length str) 1)))
      (while (>= end 0)
        (when (bash-completion--find-last (aref str end) wordbreaks)
	  (throw 'bash-completion-return
                 (list (substring str 0 (1+ end))
                       (substring str (1+ end))
                       (aref str end))))
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
      (let ((process) (oldterm (getenv "TERM")) (cleanup t) (bash-major-version))
        (unwind-protect
            (progn
              (setenv "TERM" "dumb")
              (setenv "EMACS_BASH_COMPLETE" "t")
              (let* ((start-proc-fun (if remote #'start-file-process #'start-process))
                     (buffer-name (generate-new-buffer-name " bash-completion"))
                     (args `("*bash-completion*"
                             ,buffer-name
                             ,(if remote bash-completion-remote-prog bash-completion-prog)
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
              (if remote
                  ;; Set EMACS_BASH_COMPLETE now for remote
                  ;; completion, since setenv doesn't work. This will
                  ;; unfortunately not be available in .bashrc or
                  ;; .bash_profile. TODO: Find a way of getting it to
                  ;; work from the very beginning.
                  (process-send-string process "EMACS_BASH_COMPLETE=t\n"))
              (dolist (start-file bash-completion-start-files)
                (when (file-exists-p (bash-completion--expand-file-name start-file))
                  (process-send-string process (concat ". " start-file "\n"))))
              (process-send-string
               process
               (concat
                ;; attempt to turn off unexpected status messages from
                ;; bash if the current version of bash does not
                ;; support these options, the commands will fail
                ;; silently and be ignored.
                "shopt -u checkjobs\n"
                "shopt -u mailwarn\n"
                "export MAILCHECK=-1\n"
                "export -n MAIL\n"
                "export -n MAILPATH\n"
                "unset HISTFILE\n"
                ;; some bash completion functions use quote_readline
                ;; to double-quote strings - which compgen understands
                ;; but only in some environment. disable this dreadful
                ;; business to get a saner way of handling spaces.
                ;; Noticed in bash_completion v1.872.
                "function quote_readline { echo \"$1\"; }\n"))

              (bash-completion-send "PROMPT_COMMAND='';PS1='\t$?\v'" process bash-completion-initial-timeout)
              (bash-completion-send "complete -p" process)
              (process-put process 'complete-p
                           (bash-completion-build-alist (process-buffer process)))
              (bash-completion-send "echo -n ${BASH_VERSINFO[0]}" process)
              (setq bash-major-version
                    (with-current-buffer (process-buffer process)
                      (string-to-number (buffer-substring-no-properties
                                         (point-min) (point-max)))))
              (bash-completion-send
               (concat "function __bash_complete_wrapper {"
                       (if (>= bash-major-version 4)
                           " COMP_TYPE=9; COMP_KEY=9; _EMACS_COMPOPT=\"\";"
                         "")
                       " eval $__BASH_COMPLETE_WRAPPER;"
                       " n=$?;"
                       " if [[ $n = 124 ]]; then"
                       (bash-completion--side-channel-data
                        "wrapped-status" "124")
                       "  return 1; "
                       " fi; "
                       (when (>= bash-major-version 4)
                         (concat " if [[ -n \"${_EMACS_COMPOPT}\" ]]; then"
                                 (bash-completion--side-channel-data
                                  "compopt" "${_EMACS_COMPOPT}")
                                 " fi;"))
                       " return $n;"
                       "}")
               process)
              (if (>= bash-major-version 4)
                  (bash-completion-send
                   (concat
                    "function compopt {"
                    " command compopt \"$@\" 2>/dev/null;"
                    " ret=$?; "
                    " if [[ $ret == 1 && \"$*\" = *\"-o nospace\"* ]]; then"
                    "  _EMACS_COMPOPT='-o nospace';"
                    "  return 0;"
                    " fi;"
                    " if [[ $ret == 1 && \"$*\" = *\"+o nospace\"* ]]; then"
                    "  _EMACS_COMPOPT='+o nospace';"
                    "  return 0;"
                    " fi;"
                    " return $ret; "
                    "}")
                   process))
              (bash-completion-send "echo -n ${COMP_WORDBREAKS}" process)
              (process-put process 'wordbreaks
                           (with-current-buffer (process-buffer process)
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
              (process-put process 'bash-major-version bash-major-version)
              (push (cons remote process) bash-completion-processes)
              (setq cleanup nil)
              process)
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
  "Parse the content of BUFFER into an alist.

BUFFER should contains the output of:
  complete -p

The returned alist is a sligthly parsed version of the output of
\"complete -p\"."
  (let ((alist (list)))
    (with-current-buffer buffer
      (save-excursion
        (setq alist nil)
        (goto-char (point-max))
        (while (= 0 (forward-line -1))
          (let ((words (bash-completion-strings-from-tokens
                        (bash-completion-tokenize
                         (line-beginning-position)
                         (line-end-position)))))
            (when (string= "complete" (car words))
              (if (member "-D" (cdr words))
                  ;; default completion 
                  (push (cons nil (delete "-D" (cdr words))) alist)
                ;; normal completion
                (let* ((reverse-wordsrest (nreverse (cdr words)))
                       (command (car reverse-wordsrest))
                       (options (nreverse (cdr reverse-wordsrest))) )
                  (when (and command options)
                    (push (cons command options) alist)))))))))
    alist))

(defun bash-completion--customize (comp process &optional nodefault)
  (unless (eq 'command (bash-completion--type comp))
    (let ((compgen-args-alist
           (process-get process 'complete-p))
          (command-name (bash-completion--command comp)))
      ;; TODO: first lookup the full command path, then only the
      ;; command name.
      (setf (bash-completion--compgen-args comp)
            (or (cdr (assoc command-name compgen-args-alist))
                (and (not nodefault) (cdr (assoc nil compgen-args-alist))))))))

(defun bash-completion-generate-line (comp)
  "Generate a command-line that calls compgen for COMP.

COMP is a struct returned by `bash-completion--parse'. It is
normally configured using `bash-completion--customize' before
calling this command.

If the compgen argument set specifies a custom function or command, the
arguments will be passed to this function or command as:
 COMP_LINE, taken from (bash-completion--line COMP)
 COMP_POINT, taken from (bash-completion--point COMP)
 COMP_WORDS, taken from (bash-completion--words COMP) (a bash array)
 COMP_CWORD, taken for (bash-completion--cword COMP)

Return a cons containing the completion type (command default or
custom) and a bash command-line that calls compgen to get the
completion candidates."
  (let ((quoted-stub (bash-completion-quote (bash-completion--stub comp)))
        (completion-type (bash-completion--type comp))
        (compgen-args (bash-completion--compgen-args comp)))
    (concat
     (bash-completion-cd-command-prefix)
     (cond
      ((eq 'command completion-type)
       (concat "compgen -b -c -a -A function -- " quoted-stub))

      ((eq 'default completion-type)
       (concat "compgen -o default -- " quoted-stub))

      ((and (eq 'custom completion-type) (or (member "-F" compgen-args)
                                             (member "-C" compgen-args)))
       ;; custom completion with a function of command
       (let* ((args (copy-tree compgen-args))
              (function (or (member "-F" args) (member "-C" args)))
              (function-name (car (cdr function))))
         (setcar function "-F")
         (setcar (cdr function) "__bash_complete_wrapper")
         (format "__BASH_COMPLETE_WRAPPER=%s compgen %s -- %s"
                 (bash-completion-quote
                  (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s %s %s %s"
                          (bash-completion-quote (bash-completion--line comp))
                          (bash-completion--point comp)
                          (bash-completion--cword comp)
                          (bash-completion-join (bash-completion--words comp))
                          (bash-completion-quote function-name)
                          (bash-completion-quote (bash-completion--command comp))
                          (bash-completion-quote (bash-completion--stub comp))
                          (bash-completion-quote (or (nth (1- (bash-completion--cword comp))
                                                          (bash-completion--words comp))
                                                     ""))))
                 (bash-completion-join args)
                 quoted-stub)))
      ((eq 'custom completion-type)
       ;; simple custom completion
       (format "compgen %s -- %s"
               (bash-completion-join compgen-args)
               quoted-stub))
      (t (error "Unsupported completion type: %s" completion-type)))
     " 2>/dev/null")))

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
         (proc (cdr entry)))
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
  (process-buffer (bash-completion-require-process)))

(defun bash-completion-is-running ()
  "Check whether the bash completion process is running."
  (let* ((entry (assoc (file-remote-p default-directory)
                       bash-completion-processes))
         (proc (cdr entry))
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
  (let ((process (or process (bash-completion-require-process)))
	(timeout (or timeout bash-completion-process-timeout)))
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (process-send-string process (concat commandline "\n"))
      (while (not (progn (goto-char 1) (search-forward "\v" nil t)))
	(unless (accept-process-output process timeout)
	  (error (concat
                  "Timeout while waiting for an answer from "
                  "bash-completion process.\nProcess output: <<<EOF\n%sEOF")
                 (buffer-string))))
      (let* ((control-v-position (point))
	     (control-t-position (progn (search-backward "\t" nil t) (point)))
	     (status-code (string-to-number
			   (buffer-substring-no-properties
			    (1+ control-t-position) (1- control-v-position)))))
	(delete-region control-t-position (point-max))
	;; (message "status: %d content: \"%s\""
	;; 	 status-code
	;; 	 (buffer-substring-no-properties
	;; 	  (point-min) (point-max)))
        (if (string=
               "124"
               (bash-completion--parse-side-channel-data "wrapped-status"))
            124
          status-code)))))

(defun bash-completion--get-output (process)
  "Return the output of the last command sent through `bash-completion-send'."
  (with-current-buffer (process-buffer process) (buffer-string)))

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

(defun bash-completion--has-compgen-option (compgen-args option-name)
  "Check whether COMPGEN-ARGS contains -o OPTION-NAME."
  (let ((rest compgen-args) (found))
    (while (and (not found)
                (setq rest (cdr (member "-o" rest))))
      (when (string= option-name (car rest))
        (setq found t))
      (setq rest (cdr rest)))
    found))

(defun bash-completion--side-channel-data (name value)
  "Return an echo command that outputs NAME=VALUE as side-channel data.

Parse that data from the buffer output using
`bash-completion--side-channel-data'."
  (format " echo -n \"\e\e%s=%s\e\e\";" name value))

(defun bash-completion--parse-side-channel-data (name)
  "Parse side-channel data NAME from the current buffer.

This parses data added by `bash-completion--side-channel-data'
being run by the shell and removes it from the buffer.

Return the parsed value, as a string or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward-regexp
             (format "\e\e%s=\\([^\e]*\\)\e\e"
                     (regexp-quote name))
             nil 'noerror)
        (prog1 (match-string 1)
          (delete-region (match-beginning 0) (match-end 0)))))))

(provide 'bash-completion)
;;; bash-completion.el ends here
