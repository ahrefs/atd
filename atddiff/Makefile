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

# Update the output format of atddiff by running 'make types'.
# This requires an external installation of the atdgen command.
.PHONY: types
types:
	$(MAKE) -C src/lib types
