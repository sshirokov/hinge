#-*- mode:makefile-gmake; -*-
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d
ASDF_CONF = (:directory \"$(ROOT)/\")
ASDF_CONF_NAME = $(REGISTRYD)/"$(TARGET)-02.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"$(TARGET)-01.conf"

.PHONY: asdf asdf-clean

asdf: | $(REGISTRYD) $(ASDF_CONF_NAME) $(VENDOR_ASDF_CONF_NAME)
	@echo "Added $(TARGET) in $(ROOT) to ASDF registry"

asdf-clean:
	@echo "=> Cleaning up after ASDF"
	rm -f $(ASDF_CONF_NAME)
	rm -f $(VENDOR_ASDF_CONF_NAME)

$(REGISTRYD):
	@echo "=> Creating ASDF registry configuration directory"
	mkdir -p $(REGISTRYD)

$(ASDF_CONF_NAME):
	@echo "=> Installing: $(ASDF_CONF_NAME)"
	echo "$(ASDF_CONF)" > $(ASDF_CONF_NAME)

$(VENDOR_ASDF_CONF_NAME):
	@echo "=> Installing: $(VENDOR_ASDF_CONF_NAME)"
	echo "$(VENDOR_ASDF_CONF)" > $(VENDOR_ASDF_CONF_NAME)
