#
# OCaml/JSON backend using the Yojson AST
#

DUNE ?= dune

.PHONY: build
build:
	$(DUNE) build @all
	# Install atdml into $ROOT/local/bin/
	dune install --prefix local

# Running the tests requires the latest atdml executable in ../local/bin/
.PHONY: test
test: build
	$(DUNE) build tests/test.exe
	./test

.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf bin
