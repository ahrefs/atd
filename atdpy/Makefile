#
# Python-mypy/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	$(MAKE) clean-for-dune
	$(DUNE) build @all

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	$(DUNE) runtest -f; status=$$?; \
	  ln -s ../../../_build/default/atdpy/test/python-tests/everything.py \
	    test/python-tests/everything.py && \
	  exit "$$status"

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/python-tests/everything.py

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
