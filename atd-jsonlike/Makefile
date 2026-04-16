DUNE ?= dune

.PHONY: build
build:
	$(DUNE) build

.PHONY: test
test: build
	$(DUNE) build tests/test.exe
	./test
