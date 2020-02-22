include ../dev/tools.mk

PACKAGE_NAME ?= $(notdir $(CURDIR))


.PHONY: test
test: cabal-fmt
	$(CABAL) new-test

.PHONY: build
build: cabal-fmt
	$(CABAL) new-build

.PHONY: cabal-fmt
cabal-fmt: $(PACKAGE_NAME).cabal
	$(CABAL_FMT) -i $<


include ../dev/commands.mk
