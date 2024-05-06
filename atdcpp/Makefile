#
# Dlang/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdcpp
	$(MAKE) clean-for-dune
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdcpp bin/atdcpp

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	$(DUNE) runtest -f; status=$$?; \
	  ln -s ../../../_build/default/atdcpp/test/cpp-tests/everything_atd.hpp \
	    test/cpp-tests/everything_atd.hpp && \
	  ln -s ../../../_build/default/atdcpp/test/cpp-tests/everything_atd.cpp \
	    test/cpp-tests/everything_atd.cpp && \
	  exit "$$status"

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/cpp-tests/everything_atd.hpp
	rm -f test/cpp-tests/everything_atd.cpp

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
	rm -rf bin
