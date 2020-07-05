# bash-completion for Emacs [![test](https://github.com/szermatt/emacs-bash-completion/workflows/test/badge.svg)](https://github.com/szermatt/emacs-bash-completion/actions) [![melpa](https://melpa.org/packages/bash-completion-badge.svg)](https://melpa.org/#/bash-completion) [![melpa-stable](https://stable.melpa.org/packages/bash-completion-badge.svg)](https://stable.melpa.org/#/bash-completion)


bash-completion.el defines dynamic completion hooks for shell-mode
and shell-command prompts that is based on bash completion.

Bash completion for Emacs:

- is aware of bash builtins, aliases and functions
- does file expansion inside of colon-separated variables
  and after redirections (> or <)
- escapes special characters when expanding file names
- is configurable through programmable bash completion
- works on remote shells, through TRAMP.

A simpler and more complete alternative to bash-completion.el is to
run a bash shell in a buffer in term mode (`M-x ansi-term`).
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
`shell-dynamic-complete-functions`.

For example:

```elisp
        (autoload 'bash-completion-dynamic-complete
          "bash-completion"
          "BASH completion hook")
        (add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
```

or simpler, but forces you to load bash-completion at startup:

```elisp
        (require 'bash-completion)
        (bash-completion-setup)
```

After that reload your .emacs (`M-x eval-buffer`) or restart.

When called from a bash shell buffer,
`bash-completion-dynamic-complete` communicates with the current shell
to reproduce, as closely as possible the normal bash auto-completion,
available on full terminals.

When called from non-shell buffers, such as the prompt of M-x compile,
`bash-completion-dynamic-complete` creates a separate bash process
just for doing completion. Such processes have the environment
variable `EMACS_BASH_COMPLETE` set to `t`, to help distinguish them
from normal shell processes.

### Completion at point

Additionally, you can enable bash completion in any buffer that contains bash 
commands. To do that, call 
```elisp
(bash-completion-dynamic-complete-nocomint COMP-START COMP-POS DYNAMIC-TABLE)
``` 
from a function added to `completion-at-point-functions`. 

The trickiest part is setting COMP-START to where the bash command starts;
It depends on the mode of the calling buffer and might, in some cases, span 
multiple lines.

COMP-POS is usually the current position of the cursor.

When calling from `completion-at-point`, make sure to pass a non-nil value 
to the DYNAMIC-TABLE argument so it returns a function instead of a list
of strings. This isn't just an optimization: returning a function instead 
of a list tells Emacs it should avoids post-filtering the results and 
possibly discarding useful completion from bash.

For example, here's a function to to do bash completion from an 
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

## CONTRIBUTING

To report bugs, features or even to ask questions, please open an [issue](https://github.com/szermatt/emacs-bash-completion/issues). To contribute code or documentation, please open a [pull request](https://github.com/szermatt/emacs-bash-completion/pulls). 

See [CONTRIBUTING.md](CONTRIBUTING.md) for more details. 

## COMPATIBILITY

bash-completion.el is known to work with Bash 3, 4 and 5, on Emacs,
starting with version 24.3, under Linux and OSX. It does not work on
XEmacs.
