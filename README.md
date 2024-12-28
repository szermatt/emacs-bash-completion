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

However, bash-completion.el only works with bash. If you run
other shells or other interactive programs that support completion,
bash-completion will not be able to help.

A more powerful alternative to bash-completion.el is [MisTTY],
as it works with all shells and most interactive programs that support
completion. On the other hand, MisTTY cannot integrate with Emacs
completion and is not able to provide completion in shell-command
prompts nor in eshell mode.

> [!NOTE]
> While I'm still maintaining bash-completion.el, I've switched
> to [MisTTY] for day-to-day operation, so that package is more
> likely to receive updates. -- szermatt

[MisTTY]: http://github.com/szermatt/mistty

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

You can also use bash completion as an additional completion function
in any buffer that contains bash commands. To do that, add
`bash-completion-capf-nonexclusive` to the buffer-local
`completion-at-point-functions`. For example, you can setup bash
completion in `eshell-mode` by invoking

```elisp
(add-hook 'eshell-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'bash-completion-capf-nonexclusive nil t)))
```

There is also a lower-level function
`bash-completion-dynamic-complete-nocomint` which allows you to
construct your own `completion-at-point` function.

```elisp
(bash-completion-dynamic-complete-nocomint COMP-START COMP-POS DYNAMIC-TABLE)
```

COMP-START is where the bash command starts --- it depends on the mode
of the calling buffer. In most cases, `line-beginning-position` works
because it uses `field` boundaries.

COMP-POS is usually the current position of the cursor.

When calling from `completion-at-point`, make sure to pass a non-nil
value to the DYNAMIC-TABLE argument so it returns a function instead
of a list of strings. This isn't just an optimization: returning a
function instead of a list tells Emacs it should avoids post-filtering
the results and possibly discarding useful completion from bash.

## TROUBLESHOOTING

If completion in a bash shell doesn't behave as you think it should, check
the following:

* Does bash behave differently when run outside of Emacs? If not, check
  your shell configuration.
* Call `M-x bash-completion-debug` and look at the `output-buffer`
  section. Does it  match the expected set of completion? If yes,
  it might be a display problem. Are you using a completion engine
  other than the default, such as ivy or helm? Try turning it off to
  confirm, then [file a bug](
    https://github.com/szermatt/emacs-bash-completion/issues/new).
* If all else fails, [file a bug](
  https://github.com/szermatt/emacs-bash-completion/issues/new). Please
  include the output of `M-x bash-completion-debug`, the command you're
  trying to use and the function or package providing completion for
  that command and where to download it.

## CONTRIBUTING

To report bugs, features, please open an [issue](
https://github.com/szermatt/emacs-bash-completion/issues/new). To ask 
questions or just for general comments, add a new [discussion](
https://github.com/szermatt/emacs-bash-completion/discussions). To 
contribute code or documentation, please open a [pull request](
https://github.com/szermatt/emacs-bash-completion/pulls).

See [CONTRIBUTING.md](CONTRIBUTING.md) for more details.

## COMPATIBILITY

bash-completion.el is known to work with Bash 4.2 and later and Bash
5, on Emacs, starting with version 25.3, under Linux and OSX.
