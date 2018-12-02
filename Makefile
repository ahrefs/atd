DUNE ?= dune

all:
	$(DUNE) build

test:
	$(DUNE) runtest

check: test

js:
	$(DUNE) build atdgen/bin/ag_main.bc.js

clean:
	$(DUNE) clean

all-supported-ocaml-versions:
	$(DUNE) runtest --workspace dune-workspace.dev

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

.PHONY: all test clean check doc livedoc
