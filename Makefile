CASK ?= cask
EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} build

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
