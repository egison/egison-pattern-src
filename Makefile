include dev/tools.mk

PACKAGES := $(wildcard egison-pattern-src*)


.PHONY: test
test: cabal-fmt
	$(CABAL) new-test all

.PHONY: build
build: cabal-fmt
	$(CABAL) new-build all

.PHONY: cabal-fmt
cabal-fmt: $(foreach pkg,$(PACKAGES),$(pkg)/$(pkg).cabal)
	$(CABAL_FMT) -i $^

.PHONY: clean
clean:
	$(CABAL) new-clean
