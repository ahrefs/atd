.PHONY: default build test install uninstall clean
default: build

ifndef PREFIX
  PREFIX = $(HOME)
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif

build:
	omake
test:
	omake test
install:
	cp atdj.opt $(BINDIR)/atdj
uninstall:
	rm -f $(BINDIR)/atdj
clean:
	omake clean
