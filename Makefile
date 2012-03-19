#-*- mode:makefile-gmake; -*-
ROOT ?= $(shell pwd)
README ?= $(ROOT)/README.md
TARGET ?= hinge

include $(ROOT)/make/*.mk


define HELP
	echo "Interesting targets:"
	echo "--------------------"
	echo "make help"
	echo "  This noise"
	echo
	echo "make develop"
	echo "  Configures the submodules, vendor packages, user lisp dependencies"
	echo "  and anything else needed to actually boot the package"
	echo
	echo "Info:"
	echo "-----"
	echo "The make driven chain is in $(ROOT)/make."
endef
.DEFAULT_GOAL=help
.PHONY: help
help:
	@$(HELP)
