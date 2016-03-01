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

.PHONY: findlib-install findlib-uninstall
findlib-install:
	$(MAKE) -C src findlib-install

findlib-uninstall:
	$(MAKE) -C src findlib-uninstall

.PHONY: exe-install exe-uninstall
exe-install:
	$(MAKE) -C src exe-install
	$(MAKE) -C atdgen-cppo exe-install

exe-uninstall:
	$(MAKE) -C src exe-uninstall
	$(MAKE) -C atdgen-cppo exe-uninstall

install:
	$(MAKE) -C src install
	$(MAKE) -C atdgen-cppo install

uninstall:
	$(MAKE) -C src uninstall
	$(MAKE) -C atdgen-cppo uninstall

reinstall:
	$(MAKE) -C src reinstall
	$(MAKE) -C atdgen-cppo reinstall

test:
	$(MAKE) -C test

test-all:
	$(MAKE) -C test test-all

clean:
	rm -f *~ util/*~ example/*~
	$(MAKE) -C src clean
	$(MAKE) -C test clean
	$(MAKE) -C atdgen-cppo clean
