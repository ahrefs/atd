#
# OCaml/JSON backend using the Yojson AST
#

DUNE ?= dune

.PHONY: build
build:
	rm -f atdml
	$(DUNE) build @all
	ln -s ../_build/default/atdml/src/bin/Atdml_main.exe atdml

.PHONY: test
test:
	$(DUNE) build tests/test.exe
	./test

.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf bin
