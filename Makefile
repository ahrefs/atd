DUNE ?= dune

all:
	$(DUNE) build

tests:
	$(DUNE) runtest

check: tests

clean:
	$(DUNE) clean

all-supported-ocaml-versions:
	$(DUNE) runtest  --workspace dune-workspace.dev

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

.PHONY: all tests clean check doc livedoc
