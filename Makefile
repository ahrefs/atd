ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: default test all opt install uninstall reinstall clean
default:
	$(MAKE) -C src

all:
	$(MAKE) -C src all
opt:
	$(MAKE) -C src opt

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

reinstall:
	$(MAKE) -C src reinstall

test:
	$(MAKE) -C test

clean:
	rm -f *~ util/*~ example/*~
	$(MAKE) -C src clean
	$(MAKE) -C test clean
