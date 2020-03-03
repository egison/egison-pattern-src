# TODO: workaround for -XCPP
.PHONY: fmt
fmt:
	find . -name '*.hs' \
		-not -path '*/Language/Egison/Parser/Pattern/Prim/Parse.hs' \
		-exec $(BRITTANY) --write-mode=inplace {} +

.PHONY: lint
lint:
	$(HLINT) .

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
