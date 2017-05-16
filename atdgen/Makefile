JBUILDER ?= jbuilder

all:
	$(JBUILDER) build

tests:
	$(JBUILDER) runtest

check: tests

clean:
	rm -rf _build *.install

.PHONY: all tests clean check
