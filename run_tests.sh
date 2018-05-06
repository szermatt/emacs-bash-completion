#!/bin/sh
#
# Compiles and runs test using the emacs binary specified by
# ${EMACSCMD}, which defaults to emacs, and the bash binary specified
# by ${BASHCMD}, which defaults to bash.
#
rootdir="$(dirname "$0")"
testdir="${rootdir}/test"
emacscmd="${EMACSCMD:-emacs}"
bashcmd="${BASHCMD:-$(which bash)}"
echo "Testing against ${emacscmd} and ${bashcmd}:"
"${emacscmd}" \
     -Q \
     -batch \
     -eval "(setq byte-compile-error-on-warn t)" \
     -L "${rootdir}" \
     -f batch-byte-compile \
     "${rootdir}/bash-completion.el" \
    || exit 1 
exec "${emacscmd}" \
    -Q \
    -batch \
    -eval "(setq bash-completion-prog \"${bashcmd}\")" \
    -L "${rootdir}" \
    -L "${testdir}" \
    -l "${testdir}/bash-completion-test.el" \
    -l "${testdir}/bash-completion-integration-test.el" \
    -f ert-run-tests-batch-and-exit
