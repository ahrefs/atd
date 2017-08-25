JBUILDER ?= jbuilder

all:
	$(JBUILDER) build

tests:
	$(JBUILDER) runtest

check: tests

clean:
	$(JBUILDER) clean

.PHONY: all tests clean check

.PHONY: release
release:
	./release.sh
