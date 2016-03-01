ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: default demo clean install uninstall reinstall
.PHONY: exe-install exe-uninstall

default: demo

demo:
	@echo "--- Demo ---"
	ocamlfind opt -o example \
          -pp cppo-json \
          -package atdgen -linkpkg \
          example.ml
	./example

exe-install:
	cp atdgen-cppo cppo-json "$(BINDIR)"

exe-uninstall:
	rm -f "$(BINDIR)"/atdgen-cppo "$(BINDIR)"/cppo-json

install: exe-install
uninstall: exe-uninstall

reinstall:
	$(MAKE) uninstall BINDIR="$(BINDIR)"
	$(MAKE) install BINDIR="$(BINDIR)"

clean:
	rm -f *.cm[iox] *.o *~ example
