#-*- mode:makefile-gmake; -*-
.PHONY: all clean init
init:
	@echo "=> Updating submodules"
	(cd .. ; git submodule update --init --recursive;)

all: | init
	@echo "=> Nothing to do for vendor."

clean:
	find . -name '*.fasl' -delete

