;;; bash-completion.el --- BASH completion for the shell buffer -*- lexical-binding: t -*-

;; Copyright (C) 2009 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 3.0.0
;; Keywords: shell bash bash-completion
;; URL: http://github.com/szermatt/emacs-bash-completion
;; Package-Requires: ((emacs "24.1"))

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
;; Bash completion can also be run programmatically, outside of a
;; shell-mode command, by calling
;; `bash-completion-dynamic-complete-nocomint'
;;
;; INSTALLATION
;;
;; 1. copy bash-completion.el into a directory that's on Emacs load-path
;; 2. add this into your .emacs file:
;;   (setq bash-completion-use-separate-processes nil)
;;   (autoload 'bash-completion-dynamic-complete \"bash-completion\"
;;     \"BASH completion hook\")
;;   (add-hook 'shell-dynamic-complete-functions
;;      'bash-completion-dynamic-complete)
;;
;;   or simpler, but forces you to load this file at startup:
;;
;;   (require 'bash-completion)
;;   (setq bash-completion-use-separate-processes nil)
;;   (bash-completion-setup)
;; 
;;   NOTE: Setting `bash-completion-use-separate-processes` to nil on new
;;   installations is recommended. It might become the default in future
;;   versions of `bash-completion.el. See the section
;;   [bash-completion-use-separate-processes](#bash-completion-use-separate-processes)
;;   for more details.
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
;; CAVEAT
;; 
;; TL;DR Set `bash-completion-use-separate-processes` to `nil` and avoid
;; the issues and complications described in this section.
;; 
;; When `bash-completion-use-separate-processes` is `t`, completion runs
;; in a separate process from the shell process. 
;; 
;; This might be useful in some cases, as it allows interrupting slow
;; completions, when necessary.
;; 
;; However using a separate process for doing the completion has several
;; important disadvantages:
;; 
;; - bash completion is slower than standard emacs completion
;; - it relies on directory tracking working correctly on Emacs
;; - the first completion can take a long time, since a new bash process
;;   needs to be started and initialized
;; - the separate process is not aware of any changes made to bash
;;   in the current buffer.
;;   In a standard terminal, you could do:
;; 
;;         $ alias myalias=ls
;;         $ myal<TAB>
;; 
;;   and bash would propose the new alias.
;;   Bash-completion.el cannot do that, as it is not aware of anything
;;   configured in the current shell. To make bash-completion.el aware
;;   of a new alias, you need to add it to .bashrc and restart the
;;   completion process using `bash-completion-reset'.
;; 
;; When using separate processes, right after enabling programmable bash
;; completion, and whenever you make changes to you .bashrc, call
;; `bash-completion-reset' to make sure bash completion takes your new
;; settings into account.
;; 
;; Loading /etc/bash_completion often takes time, and is not necessary
;; in shell mode, since completion is done by a separate process, not
;; the process shell-mode process.
;; 
;; To turn off bash completion when running from emacs but keep it on
;; for processes started by bash-completion.el, add this to your .bashrc:
;; 
;;     if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
;;          -f /etc/bash_completion ]]; then
;;       . /etc/bash_completion
;;     fi
;; 
;; Emacs sets the environment variable INSIDE_EMACS to the processes
;; started from it. Processes started by bash-completion.el have
;; the environment variable EMACS_BASH_COMPLETE set to t.
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

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'shell)

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

(defcustom bash-completion-use-separate-processes t
  "Enable/disable the use of separate processes to perform completion.

When set to a non-nil value, separate processes will be used to
perform completion. If nil, the process associated with the
current buffer is used to perform completion. Even when this
variable is set to nil, a separate process can be used to perform
completion when:
- the current buffer is not a comint buffer
- no bash process is associated with the current buffer
- an error occurred while trying to get completions"
  :type 'boolean
  :group 'bash-completion)

(defcustom bash-completion-prog (executable-find "bash")
  "Name or path of the BASH executable to run for command-line completion.

This should be either an absolute path to the BASH executable or
the name of the bash command if it is on Emacs' PATH. This should
point to a recent version of BASH, 3 or 4, with support for
command-line completion.

This variable is only used when creating separate processes for
performing completion. See
`bash-completion-use-separate-processes' for further
explanation."
  :type '(file :must-match t)
  :group 'bash-completion)

(defcustom bash-completion-remote-prog "bash"
  "Name or path of the remote BASH executable to use.

This is the path of an BASH executable available on the remote machine.
Best is to just specify \"bash\" and rely on the PATH being set correctly
for the remote connection.

This variable is only used when creating separate processes for
performing completion. See
`bash-completion-use-separate-processes' for further
explanation."
  :type '(string)
  :group 'bash-completion)

(defcustom bash-completion-args '("--noediting")
  "Args passed to the BASH shell.

This variable is only used when creating separate processes for
performing completion. See
`bash-completion-use-separate-processes' for further
explanation."
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
which typically takes a long time.

This variable is only used when creating separate processes for
performing completion. See
`bash-completion-use-separate-processes' for further
explanation."
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
  "Shell files that, if they exist, will be sourced at the beginning of a bash completion subprocess.

This variable is only used when creating separate processes for
performing completion. See
`bash-completion-use-separate-processes' for further
explanation.")

(defvar bash-completion-wordbreaks ""
  "Extra wordbreaks to use when tokenizing, in `bash-completion-tokenize'.")

(defvar bash-completion-output-buffer " *bash-completion*"
  "Buffer storing completion results.

This variable is not used when
`bash-completion-use-separate-processes' is non-nil.")

;;; ---------- Internal variables and constants

(defvar bash-completion-processes nil
  "Bash processes alist.

Mapping between remote paths as returned by `file-remote-p' and
Bash processes.")

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
(cl-defstruct (completion (:constructor bash-completion--make)
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
  compopt        ; options forced with compopt, same format as compgen-args
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

(defun bash-completion--option (comp optname)
  "Returns the value of a compopt option OPTNAME to use for COMP.

The option can be passed to the complete builtin or set later in
the completion script using compopt."
  (bash-completion--has-compgen-option
   (append (bash-completion--compgen-args comp)
           (bash-completion--compopt comp))
   optname))

(defun bash-completion--has-compgen-option (compgen-args optname)
  "Check whether COMPGEN-ARGS has OPTNAME set. 

COMPGEN-ARGS is a list of options for the complete builtin."
  (let ((last-arg nil) (result nil))
    (dolist (arg compgen-args)
      (when (equal optname arg)
        (cond
         ((equal last-arg "-o") (setq result t))
         ((equal last-arg "+o") (setq result nil))))
      (setq last-arg arg))
    result))

(defun bash-completion--command (comp)
  "Return the current command for the completion, if there is one."
  (file-name-nondirectory (car (bash-completion--words comp))))

(defun bash-completion--get-buffer (process)
  "Return the buffer used to store completion results.

PROCESS is the bash process from which completions are
retrieved. When `bash-completion-use-separate-processes' is nil,
PROCESS is not used and `bash-completion-output-buffer' is
returned."
  (if bash-completion-use-separate-processes
      (process-buffer process)
    (get-buffer-create bash-completion-output-buffer)))

(defun bash-completion--setup-bash-common (process)
  "Setup PROCESS to be ready for completion."
  (let (bash-major-version)
    (bash-completion-send "complete -p" process)
    (process-put process 'complete-p
                 (bash-completion-build-alist (bash-completion--get-buffer process)))
    (bash-completion-send "echo -n ${BASH_VERSINFO[0]}" process)
    (setq bash-major-version
          (with-current-buffer (bash-completion--get-buffer process)
            (string-to-number (buffer-substring-no-properties
                               (point-min) (point-max)))))
    (bash-completion-send
     (concat "function __emacs_complete_wrapper {"
             (if (>= bash-major-version 4)
                 " COMP_TYPE=9; COMP_KEY=9; declare -A _EMACS_COMPOPT=( );"
               "")
             " eval $__EMACS_COMPLETE_WRAPPER;"
             " n=$?;"
             " if [[ $n = 124 ]]; then"
             (bash-completion--side-channel-data
              "wrapped-status" "124")
             "  return 1; "
             " fi; "
             (when (>= bash-major-version 4)
               (concat " local opt;"
                       " local allopts='';"
                       " for opt in \"${!_EMACS_COMPOPT[@]}\"; do"
                       "   allopts=\"$allopts ${_EMACS_COMPOPT[$opt]} $opt\";"
                       " done; "
                       (bash-completion--side-channel-data "compopt" "${allopts}")))
             " return $n;"
             "}")
     process)
    (if (>= bash-major-version 4)
        (bash-completion-send
         (concat
          "function compopt {"
          " command compopt \"$@\" 2>/dev/null;"
          " ret=$?; "
          " if [[ $ret == 1 ]]; then"
          "   local s='';"
          "   local arg;"
          "   for arg in \"${@}\"; do"
          "     if [[ -n \"$s\" && \"$arg\" =~ ^[a-z]+$ ]]; then"
          "       _EMACS_COMPOPT[$arg]=$s;"
          "     fi;"
          "     if [[ \"$arg\" =~ [+-]o$ ]]; then s=${arg}; else s=''; fi;"
          "   done;"
          " fi;"
          " return $ret; "
          "}")
         process))
    
    ;; some bash completion functions use quote_readline
    ;; to double-quote strings - which compgen understands
    ;; but only in some environment. disable this dreadful
    ;; business to get a saner way of handling spaces.
    ;; Noticed in bash_completion v1.872.
    (bash-completion-send "function quote_readline { echo \"$1\"; }" process)
    
    (bash-completion-send "echo -n ${COMP_WORDBREAKS}" process)
    (process-put process 'wordbreaks
                 (with-current-buffer (bash-completion--get-buffer process)
                   (buffer-substring-no-properties
                    (point-min) (point-max))))
    (process-put process 'bash-major-version bash-major-version)

    (bash-completion-send "bind -v 2>/dev/null" process)
    (process-put process 'completion-ignore-case 
                 (with-current-buffer (bash-completion--get-buffer process)
                   (save-excursion
                     (goto-char (point-min))
                     (and (search-forward "completion-ignore-case on" nil 'noerror) t))))

    (process-put process 'setup-done t)))

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

;;;###autoload
(defun bash-completion-dynamic-complete-nocomint
    (comp-start &optional comp-pos dynamic-table)
  "Return completion information for bash command at an arbitrary position.

The bash command to be completed begins at COMP-START in the
current buffer. This must specify where the current command
starts, usually right after the prompt. 

COMP-POS is the point where completion should happen, usually
just (point). Note that a bash command can span across multiple
line, so COMP-START is not necessarily on the same line as
COMP-POS.

This function does not assume that the current buffer is a shell
or even comint buffer. It can safely be called from any buffer
where a bash command appears, including `completion-at-point'.

If DYNAMIC-TABLE is passed a non-nil value, the resulting
collection will be a function that fetches the result lazily,
when it's called.

When calling from `completion-at-point', make sure to pass a
non-nil value to DYNAMIC-TABLE. This isn't just an optimization:
returning a function instead of a list tells Emacs it should
avoids post-filtering the results and possibly discarding useful
completion from bash.

When calling from another completion engine, make sure to treat
the returned completion as reliable and not post-process them
further.

Returns (list stub-start stub-end completions) with
 - stub-start, the position at which the completed region starts
 - stub-end, the position at which the completed region ends
 - completions, a possibly empty list of completion candidates
   or a function, if DYNAMIC-TABLE is non-nil, a lambda such as the one
   returned by `completion-table-dynamic'"
  (when bash-completion-enabled
    (let ((comp-start (or comp-start (line-beginning-position)))
          (comp-pos (or comp-pos (point)))
          (bash-completion-use-separate-processes
           bash-completion-use-separate-processes)
          (process (bash-completion--get-process)))
      (when (and (not process) (not bash-completion-use-separate-processes))
        ;; no process associated with the current buffer, create a
        ;; separate completion process
        (setq bash-completion-use-separate-processes t)
        (setq process (bash-completion--get-process)))
      (let* ((comp (bash-completion--parse
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
              comp process)
           (bash-completion-comm comp process)))))))

(defun bash-completion--find-last (elt array)
  "Return the position of the last instance of ELT in array or nil."
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
  (cond
    ((string= "" word)
     "''")
    ((string-match "^[a-zA-Z0-9_./-]*$" word)
     word)
    (t
     (concat "'"
             (replace-regexp-in-string "'" "'\\''" word nil t)
             "'"))))

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
         (rebuilt-line) (stub-start) (unparsed-stub) (parsed-stub))
    ;; Note about rebuilt-line: When using readline, line and words
    ;; would be passed unquoted to the functions. This doesn't work,
    ;; however, when called from Emacs as when readline 'compgen -f'
    ;; behaves differently and does not unquote the string it's
    ;; passed. This is why words and the last word of the line are
    ;; passed unquoted. This makes the standard bash completion
    ;; scripts work - possibly at the cost of more inconsistencies
    ;; with other scripts.
    (if (or (> comp-pos end) (= start end))
        (setq stub-start comp-pos
              unparsed-stub ""
              parsed-stub ""
              words (append words '(""))
              rebuilt-line (buffer-substring-no-properties start comp-pos))
      (if (< bash-major-version 4)
          (setq last-token (car (last (bash-completion-tokenize
                                       start comp-pos wordbreaks)))))
      (setq stub-start (car (bash-completion-tokenize-get-range last-token))
            parsed-stub (bash-completion-tokenize-get-str last-token)
            unparsed-stub (buffer-substring-no-properties stub-start comp-pos)
            rebuilt-line (concat
                          (buffer-substring-no-properties
                           start (car (cdr (assq 'range (car (last line-tokens))))))
                          (cdr (assq 'str (car (last line-tokens)))))))
    (bash-completion--make
     :line rebuilt-line
     :point (length rebuilt-line)
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

Return TOKENS with new tokens found between the current point and
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
  (let* ((buffer (bash-completion--get-buffer process))
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
  (let ((output) (candidates) (compopt) (pwd))
    (with-current-buffer buffer
      (setq pwd (bash-completion--parse-side-channel-data "pwd"))
      (setq compopt (bash-completion--parse-side-channel-data "compopt"))
      (setq output (buffer-string)))
    (setq candidates (delete-dups (split-string output "\n" t)))
    (setf (bash-completion--compopt comp)
          (if compopt (delete "" (split-string compopt " "))))
    (let ((default-directory (if pwd
                                 (concat (file-remote-p default-directory) pwd)
                               default-directory)))
      (if (eq 1 (length candidates))
          (list (bash-completion-fix (car candidates) comp t))
        ;; multiple candidates
        (let ((result (list)))
          (dolist (completion candidates)
            (push (bash-completion-fix completion comp nil) result))
          (delete-dups (nreverse result)))))))

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
        (nospace (or bash-completion-nospace
                     (bash-completion--option comp "nospace")))
        (wordbreaks (bash-completion--wordbreaks comp))
        (suffix "")
        (rest) ; the part between the prefix and the suffix
        (rebuilt))
    ;; build rest by removing parsed-prefix from str
    (cond
     ((bash-completion-starts-with str parsed-prefix)
      (setq rest (substring str (length parsed-prefix))))

     ;; completion sometimes only applies to the last word, as
     ;; defined by COMP_WORDBREAKS. This detects and works around
     ;; this feature.
     ((bash-completion-starts-with
       (setq rebuilt (concat (bash-completion-before-last-wordbreak parsed-prefix wordbreaks) str))
       parsed-prefix)
      (setq rest (substring rebuilt (length parsed-prefix))))

     ;; there is no meaningful link between the prefix and the string.
     ;; Bypass the whole prefix/suffix logic and replace the string
     ;; being completed with the string provided by the completion
     ;; logic.
     ((string-match "^~.*?\\($\\|/\\)" str)
      (setq parsed-prefix (substring str 0 (match-end 0))
            unparsed-prefix
            (concat (substring str 0 (match-end 0))
                    (if open-quote (char-to-string open-quote) ""))
            rest (substring str (match-end 0))))
     
     (t
      (setq parsed-prefix ""
            unparsed-prefix (if open-quote (char-to-string open-quote) "")
            rest str)))

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

(defun bash-completion--get-or-create-separate-process ()
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
      (let (process (oldterm (getenv "TERM")) (cleanup t))
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
                ;; User's profiles can turn line editing back on,
                ;; so make sure it's off
                "set +o emacs\n"
                "set +o vi\n"))

              (bash-completion-send "PROMPT_COMMAND='' PS1='\t$?\v'" process bash-completion-initial-timeout)
              (bash-completion--setup-bash-common process)
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

(defun bash-completion--current-shell ()
  "Figure out what the shell associated with the current buffer is."
  (let ((prog (or
               (if (derived-mode-p 'shell-mode)
                   (or explicit-shell-file-name
                       (getenv "ESHELL")
                       shell-file-name))
               (let ((process (get-buffer-process (current-buffer))))
                 (when process
                   (car (process-command process)))))))
    (when prog
      (file-name-nondirectory prog))))

(defun bash-completion--get-same-process ()
  "Return the BASH process associated with the current buffer.

Return nil if the current buffer is not a comint buffer or is not
associated with a command that looks like a bash shell.
Completion will fallback to creating a separate process
completion in these cases."
  (when (derived-mode-p 'comint-mode)
    (let* ((process (get-buffer-process (current-buffer)))
           (shell (if process (bash-completion--current-shell))))
      (when (and shell (bash-completion-starts-with shell "bash"))
        (unless (process-get process 'setup-done)
          ;; The following disables the emacs and vi options. This
          ;; cannot be done by bash-completion-send as these options
          ;; interfere with bash-completion-send detecting the end
          ;; of a command. It disables prompt to avoid interference
          ;; from commands run by prompts.
          (comint-send-string
           process
           "set +o emacs; set +o vi; \
if [[ -z \"$__emacs_complete_ps1\" ]]; then \
  __emacs_complete_ps1=\"$PS1\"\
  __emacs_complete_pc=\"$PROMPT_COMMAND\"; \
fi; \
PS1='' PROMPT_COMMAND=''; history &>/dev/null -d $((HISTCMD - 1))\n")
          
          ;; The following is a bootstrap command for
          ;; bash-completion-send itself.
          (bash-completion-send
            "function __emacs_complete_pre_command { 
  if [[ -z \"$__emacs_complete_ps1\" ]]; then
    __emacs_complete_ps1=\"$PS1\"
    __emacs_complete_pc=\"$PROMPT_COMMAND\"
  fi
  PROMPT_COMMAND=__emacs_complete_prompt
  history &>/dev/null -d $((HISTCMD - 1))
}; \
function __emacs_complete_prompt {
  PS1='\t$?\v'
  PROMPT_COMMAND=__emacs_complete_recover_prompt
}; \
function __emacs_complete_recover_prompt {
  local r=$?
  PS1=\"${__emacs_complete_ps1}\"
  PROMPT_COMMAND=\"${__emacs_complete_pc}\"
  unset __emacs_complete_ps1 __emacs_complete_pc
  if [[ -n \"$PROMPT_COMMAND\" ]]; then
    (exit $r); eval \"$PROMPT_COMMAND\"
  fi
}; __emacs_complete_pre_command" process)
          (bash-completion--setup-bash-common process))
        process))))

(defun bash-completion--get-process ()
  "Setup and return a bash completion process.

If `bash-completion-use-separate-processes' is non-nil,
`bash-completion--get-or-create-separate-process' is called to
get the process, otherwise `bash-completion--get-same-process' is
used. "
  (if bash-completion-use-separate-processes
      (bash-completion--get-or-create-separate-process)
    (bash-completion--get-same-process)))

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

The returned alist is a slightly parsed version of the output of
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
     (if bash-completion-use-separate-processes
         (bash-completion-cd-command-prefix)
       (bash-completion--side-channel-data "pwd" "${PWD}"))
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
         (setcar (cdr function) "__emacs_complete_wrapper")
         (format "__EMACS_COMPLETE_WRAPPER=%s compgen %s -- %s"
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
(defun bash-completion-refresh ()
  "Force a refresh the completion table.

This can be called after changing the completion table on BASH,
or after starting a new BASH job.

This is only useful when `bash-completion-use-separate-processes'
is t."
  (interactive)
  (let* ((process (get-buffer-process (current-buffer))))
    (unless process
      (error "No process is available in this buffer."))
    (process-put process 'setup-done nil)
    (bash-completion--get-process)))
  
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
    (let ((buffer (bash-completion--get-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun bash-completion-buffer ()
  "Return the buffer of the BASH process, create the BASH process if necessary."
  (bash-completion--get-buffer (bash-completion--get-process)))

(defun bash-completion-is-running ()
  "Check whether the bash completion process is running."
  (let* ((entry (assoc (file-remote-p default-directory)
                       bash-completion-processes))
         (proc (cdr entry))
         (running (and proc (eq 'run (process-status proc)))))
    (unless (and entry running)
      (setq bash-completion-processes (delq entry bash-completion-processes)))
    running))

(defun bash-completion--output-filter (output)
  (with-current-buffer (bash-completion--get-buffer nil)
    (let ((begin (point-max)))
      (goto-char begin)
      (insert output)
      (ansi-color-filter-region begin (point))
      "")))

(defun bash-completion--wait-for-regexp (process prompt-regexp timeout &optional limit)
  (let ((no-timeout t))
    (while (and no-timeout
                (not (re-search-backward prompt-regexp limit t)))
      (setq no-timeout (accept-process-output process timeout nil t)))
    no-timeout))

(defun bash-completion-send (commandline &optional process timeout)
  "Send a command to the bash completion process.

COMMANDLINE should be a bash command, without the final newline.

PROCESS should be the bash process, if nil this function calls
`bash-completion--get-process' which might start a new process
depending on the value of
`bash-completion-use-separate-processes'.

TIMEOUT is the timeout value for this operation, if nil the value of
`bash-completion-process-timeout' is used.

Once this command has run without errors, you will find the
result of the command in the bash completion process buffer or in
`bash-completion-output-buffer' if
`bash-completion-use-separate-processes' is nil.

Return the status code of the command, as a number."
  (let ((process (or process (bash-completion--get-process)))
        (timeout (or timeout bash-completion-process-timeout))
        (comint-preoutput-filter-functions
         (if bash-completion-use-separate-processes
             comint-preoutput-filter-functions
           '(bash-completion--output-filter)))
        (send-string (if bash-completion-use-separate-processes
                         #'process-send-string
                       #'comint-send-string))
        (pre-command (unless bash-completion-use-separate-processes
                        "__emacs_complete_pre_command; ")))
    (with-current-buffer (bash-completion--get-buffer process)
      (erase-buffer)
      (funcall send-string process (concat pre-command commandline "\n"))
      (unless (bash-completion--wait-for-regexp process "\t-?[[:digit:]]+\v" timeout)
        (error (concat
                "Timeout while waiting for an answer from "
                "%s bash-completion process.\nProcess output: <<<EOF\n%sEOF")
               (if bash-completion-use-separate-processes "separate" "single")
               (buffer-string)))
      (let ((status-code (string-to-number
                          (buffer-substring-no-properties
                           (1+ (point))
                           (1- (line-end-position))))))
        (delete-region (point) (point-max))
        (if (string=
             "124"
             (bash-completion--parse-side-channel-data "wrapped-status"))
            124
          status-code)))))

(defun bash-completion--get-output (process)
  "Return the output of the last command sent through `bash-completion-send'."
  (with-current-buffer (bash-completion--get-buffer process) (buffer-string)))

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

(defun bash-completion--completion-table-with-cache (comp process)
  "Build a dynamic completion table for COMP using PROCESS.

The result is a function that works like one built by
`completion-table-with-cache' with the difference that the
completions, built by `bash-completion-comm' are complete
and that completion style doesn't necessarily use substring
completion."
  (let ((last-str) (last-result)
        (calling-buffer (current-buffer))
        (dir default-directory)
        (use-separate-process bash-completion-use-separate-processes)
        (nospace bash-completion-nospace))
    (lambda (str predicate action)
      (if (or (eq (car-safe action) 'boundaries)
              (eq action 'metadata))
          nil
        (let ((result
               (if (equal str last-str)
                   last-result
                 (let ((bash-completion-use-separate-processes use-separate-process)
                       (bash-completion-nospace nospace)
                       (default-directory dir))
                   (with-current-buffer calling-buffer
                     (bash-completion-comm comp process))))))
          (setq last-str str
                last-result result)
          (let ((filtered-result (if predicate (mapcar predicate result) result))
                (completion-ignore-case (process-get process 'completion-ignore-case)))
            (cond
             ((null action) (try-completion "" filtered-result))
             ((eq action t) filtered-result)
             (t (test-completion "" filtered-result)))))))))

(provide 'bash-completion)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; bash-completion.el ends here
