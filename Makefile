ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: default test all opt install uninstall reinstall clean findlib-install
default:
	$(MAKE) -C src

all:
	$(MAKE) -C src all
opt:
	$(MAKE) -C src opt

findlib-install:
	$(MAKE) -C src findlib-install

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

reinstall:
	$(MAKE) -C src reinstall

test:
	$(MAKE) -C test

test-all:
	$(MAKE) -C test test-all

clean:
	rm -f *~ util/*~ example/*~
	$(MAKE) -C src clean
	$(MAKE) -C test clean
