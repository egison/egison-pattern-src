CABAL ?= cabal
CABAL_FMT ?= cabal-fmt
BRITTANY ?= brittany
HLINT ?= hlint
FSWATCH ?= fswatch

.PHONY: fmt
fmt:
	find . -name '*.hs' -exec $(BRITTANY) --write-mode=inplace {} +

.PHONY: lint
lint:
	$(HLINT) .
