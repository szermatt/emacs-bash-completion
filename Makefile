CASK ?= cask
EMACS ?= emacs
BASH ?= bash
BASH_COMPLETION_RC ?= /etc/bash_completion

setup_bash=test/.set-bash-prog.el

all: test
.PHONY: all

test: clean-elc
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean-elc
.PHONY: test

unit:
	${CASK} exec ert-runner  test/bash-completion-test.el
.PHONY: unit

integration: setup_bash
	${CASK} exec ert-runner -l $(setup_bash) -p integration test/bash-completion-integration-test.el
.PHONY: integration

ert: setup_bash
	${CASK} exec ert-runner -l $(setup_bash)
.PHONY: ert

compile:
	${CASK} build
.PHONY: compile

clean-elc:
	${CASK} clean-elc
.PHONY: clean-elc

setup_bash:
	@echo '(setq bash-completion-prog "${BASH}")' >$(setup_bash)
	@echo '(setq bash-completion_test-setup-completion "${BASH_COMPLETION_RC}")' >>$(setup_bash)
.PHONY: setup_bash

