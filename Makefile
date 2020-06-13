CASK ?= cask
EMACS ?= emacs
BASH ?= bash

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	@echo '(setq bash-completion-prog "${BASH}")' >test/.set-bash-prog.el
	${CASK} exec ert-runner -l test/.set-bash-prog.el

compile:
	${CASK} build

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
