# bash-completion [![test](https://github.com/szermatt/emacs-bash-completion/workflows/test/badge.svg)](https://github.com/szermatt/emacs-bash-completion/actions) [![melpa](https://melpa.org/packages/bash-completion-badge.svg)](https://melpa.org/#/bash-completion) [![melpa-stable](https://stable.melpa.org/packages/bash-completion-badge.svg)](https://stable.melpa.org/#/bash-completion)


bash-completion.el defines dynamic completion hooks for shell-mode,
shell-command prompts and that are based on bash completion.

Bash completion for Emacs:

- is aware of bash builtins, aliases and functions
- does file expansion inside of colon-separated variables
  and after redirections (> or <)
- escapes special characters when expanding file names
- is configurable through programmable bash completion
- works on remote shells, through TRAMP.

A simpler and more complete alternative to bash-completion.el is to
run a bash shell in a buffer in term mode(M-x `ansi-term').
Unfortunately, many Emacs editing features are not available when
running in term mode.  Also, term mode is not available in
shell-command prompts.

## INSTALLATION

Copy bash-completion.el into a directory that's on Emacs load-path.
You can do that manually, or by installing it from
[MELPA](https://melpa.org/#/getting-started).

### Shell completion

To enable bash completion in shell buffers as well as in command
prompts, such as the prompt started by `compile`, add the hook
`bash-completion-dynamic-complete` to
`shell-dynamic-complete-fuctions`.

For example:

```elisp
        (setq bash-completion-use-separate-processes nil)
        (autoload 'bash-completion-dynamic-complete
          "bash-completion"
          "BASH completion hook")
        (add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
```

or simpler, but forces you to load bash-completion at startup:

```elisp
        (setq bash-completion-use-separate-processes nil)
        (require 'bash-completion)
        (bash-completion-setup)
```

  NOTE: Setting `bash-completion-use-separate-processes` to nil on new
  installations is recommended. It might become the default in future
  versions of `bash-completion.el`. See the section
  [bash-completion-use-separate-processes](#bash-completion-use-separate-processes)
  for more details.
  
After that reload your .emacs (M-x `eval-buffer') or restart.

### Completion at point

Additionally, you can enable bash completion in any buffer that contains bash 
commands. To do that, call `bash-completion-dynamic-complete-nocomint` from a 
function added to `completion-at-point-functions`. 

The tricky part is figuring out where the bash command starts, because that 
depends on  the mode of the calling buffer and might, in some cases, span 
multiple lines.

In any cases, when calling from `completion-at-point', make sure to pass a
non-nil value to DYNAMIC-TABLE argument. This isn't just an optimization:
returning a function instead of a list tells Emacs it should avoids 
post-filtering the results and possibly discarding useful completion from 
bash.

Here's, for example, a function to to do bash completion from an 
eshell buffer. To try it out, add the function below to your init file
and bind `bash-completion-from-eshell` to a custom shortcut.

```elisp
(defun bash-completion-from-eshell ()
  (interactive)
  (let ((completion-at-point-functions
         '(bash-completion-eshell-capf)))
    (completion-at-point)))

(defun bash-completion-eshell-capf ()
  (bash-completion-dynamic-complete-nocomint
   (save-excursion (eshell-bol) (point))
   (point) t))
```

## bash-completion-use-separate-processes

TL;DR Set `bash-completion-use-separate-processes` to `nil` and avoid
the issues and complications described in this section.

When `bash-completion-use-separate-processes` is `t`, completion
always runs in a separate process from the shell process. When it is 
nil and when using shell-mode, bash-completion can use the same 
bash process as shell mode, when it is available.

Running a separate process just for completion has several downsides:

- it relies on directory tracking working correctly on Emacs
- the first completion can take a long time, since a new bash process
  needs to be started and initialized
- the separate process is not aware of any changes made to bash
  in the current buffer.
  In a standard terminal, you could do:

        $ alias myalias=ls
        $ myal<TAB>

  and bash would propose the new alias.
  
  Bash-completion.el can only do that if completion and shell are
  running in the same process. 

When using separate processes, right after enabling programmable bash
completion, and whenever you make changes to you .bashrc, call
`bash-completion-reset` to make sure bash completion takes your new
settings into account.

Emacs sets the environment variable INSIDE_EMACS to the processes
started from it. Local processes started by bash-completion.el have
the environment variable EMACS_BASH_COMPLETE set to t.

## COMPATIBILITY

bash-completion.el is known to work with Bash 3 and 4, on Emacs,
starting with version 24.1, under Linux and OSX. It does not work on
XEmacs.
