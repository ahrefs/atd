JBUILDER ?= jbuilder

all:
	$(JBUILDER) build

tests:
	$(JBUILDER) runtest

check: tests

clean:
	$(JBUILDER) clean

all-supported-ocaml-versions:
	$(JBUILDER) runtest --dev --workspace jbuild-workspace.dev

.PHONY: all tests clean check
