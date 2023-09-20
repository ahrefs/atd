#
# Dlang/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdd
	$(MAKE) clean-for-dune
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdd bin/atdd

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	$(DUNE) runtest -f; status=$$?; \
	  ln -s ../../../_build/default/atdd/test/dlang-tests/everything.d \
	    test/dlang-tests/everything.d && \
	  exit "$$status"

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/dlang-tests/everything.d

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
	rm -rf bin
