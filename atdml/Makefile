#
# OCaml/JSON backend using the Yojson AST
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdml
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdml bin/atdml

.PHONY: test
test:
	$(DUNE) runtest -f

.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf bin
