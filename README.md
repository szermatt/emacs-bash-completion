bash-completion.el defines dynamic completion hooks for shell-mode and
shell-command prompts that are based on bash completion.

Bash completion for emacs:

- is aware of bash builtins, aliases and functions
- does file expansion inside of colon-separated variables
  and after redirections (> or <)
- escapes special characters when expanding file names
- is configurable through programmable bash completion

When the first completion is requested in shell model or a shell
command, bash-completion.el starts a separate bash
process.  Bash-completion.el then uses this process to do the actual
completion and includes it into Emacs completion suggestions.

A simpler and more complete alternative to bash-completion.el is to
run a bash shell in a buffer in term mode(M-x `ansi-term').
Unfortunately, many Emacs editing features are not available when
running in term mode.  Also, term mode is not available in
shell-command prompts.

## INSTALLATION

1. copy bash-completion.el into a directory that's on Emacs load-path
2. add this into your .emacs file:

        (autoload 'bash-completion-dynamic-complete 
          "bash-completion"
          "BASH completion hook")
        (add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

  or simpler, but forces you to load this file at startup:

        (require 'bash-completion)
        (bash-completion-setup)

3. reload your .emacs (M-x `eval-buffer') or restart

Once this is done, use <TAB> as usual to do dynamic completion from
shell mode or a shell command minibuffer, such as the one started
for M-x `compile'. Note that the first completion is slow, as emacs
launches a new bash process.

You'll get better results if you turn on programmable bash completion.
On Ubuntu, this means running:

    sudo apt-get install bash-completion

and then adding this to your .bashrc:

    . /etc/bash_completion

Right after enabling programmable bash completion, and whenever you
make changes to you .bashrc, call `bash-completion-reset' to make
sure bash completion takes your new settings into account.

Loading /etc/bash_completion often takes time, and is not necessary
in shell mode, since completion is done by a separate process, not
the process shell-mode process.

To turn off bash completion when running from emacs but keep it on
for processes started by bash-completion.el, add this to your .bashrc:

    if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
         -f /etc/bash_completion ]]; then
      . /etc/bash_completion
    fi

Emacs sets the environment variable INSIDE_EMACS to the processes
started from it. Processes started by bash-completion.el have
the environment variable EMACS_BASH_COMPLETE set to t.

## CAVEATS

Using a separate process for doing the completion has several
important disadvantages:

- bash completion is slower than standard emacs completion
- the first completion can take a long time, since a new bash process
  needs to be started and initialized
- the separate process is not aware of any changes made to bash
  in the current buffer.
  In a standard terminal, you could do:

        $ alias myalias=ls
        $ myal<TAB>

  and bash would propose the new alias.
  Bash-completion.el cannot do that, as it is not aware of anything
  configured in the current shell. To make bash-completion.el aware
  of a new alias, you need to add it to .bashrc and restart the
  completion process using `bash-completion-reset'.

## COMPATIBILITY

bash-completion.el is known to work on Emacs 22 through 24.4 under
Linux and OSX. It does not works on XEmacs.
