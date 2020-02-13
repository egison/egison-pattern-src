PACKAGE_NAME := egison-pattern-src

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
		$(FSWATCH) . -1 -r -e '\.git' -e 'dist-newstyle' --event Updated \
			| xargs git ls-files -cmo --exclude-standard | head -n1 \
			| tee -a /dev/stderr \
			| xargs test || continue; \
		$(MAKE) test; \
	done

.PHONY: clean
clean:
	$(CABAL) new-clean
