#
# Compare two versions of the same ATD file.
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin
	$(DUNE) build
	ln -s ../_build/install/default/bin .

.PHONY: test
test:
	$(MAKE) -C test
