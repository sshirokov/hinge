#-*- mode:makefile-gmake; -*-
LISP_BIN ?= $(shell which sbcl || echo /sbcl/does/not/exist)
LISP_PREFIX ?= CL_SOURCE_REGISTRY='$(ROOT):$(ROOT)/vendor//'
LISP ?= $(LISP_PREFIX) $(LISP_BIN)
SHUTUP = > /dev/null 2> /dev/null

.PHONY: lisp-clean lisp-fasl-clean

lisp-clean:
	@echo "=> Clearing common-lisp cache"
	rm -rf ~/.cache/common-lisp/

lisp-fasl-clean:
	@echo "=> Clearing fasls from $(ROOT)"
	find $(ROOT) -name '*.fasl' -delete

