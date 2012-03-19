#-*- mode:makefile-gmake; -*-
.PHONY: sanity-check init

sanity-check: $(ROOT)/$(TARGET).asd $(LISP_BIN) $(QL_SETUP)
	@echo "!> Environment looks sane. I'll allow this."

init: | sanity-check quicklisp
	$(MAKE) -C $(ROOT)/vendor init
	@echo "=> Environment Initialized."

develop: | init asdf
	@echo "=> You should be good to go."
