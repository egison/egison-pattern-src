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

.PHONY: watch
watch:
	while true; do \
		$(FSWATCH) . -1 -r -e '\.git' -e 'dist-newstyle' --event Updated \
			| xargs git ls-files -cmo --exclude-standard | head -n1 \
			| tee -a /dev/stderr \
			| xargs test || continue; \
		$(MAKE) test; \
	done

.PHONY: clean
clean:
	$(CABAL) new-clean
