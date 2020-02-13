PACKAGE_NAME := egison-pattern

CABAL ?= cabal
CABAL_FMT ?= cabal-fmt
BRITTANY ?= brittany
FSWATCH ?= fswatch


.PHONY: test
test: cabal-fmt
	$(CABAL) new-test

.PHONY: build
build: cabal-fmt
	$(CABAL) new-build

.PHONY: cabal-fmt
cabal-fmt: $(PACKAGE_NAME).cabal
	$(CABAL_FMT) -i $<

.PHONY: fmt
fmt:
	find src test -name '*.hs' -exec $(BRITTANY) --write-mode=inplace {} +

.PHONY: watch
watch:
	while true; do \
		$(MAKE) test; \
		$(FSWATCH) . -1 -e '\.git' -e 'dist-newstyle'; \
	done

.PHONY: clean
clean:
	$(CABAL) new-clean
