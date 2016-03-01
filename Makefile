ifndef BINDIR
  BINDIR = $$HOME/bin
endif

.PHONY: default demo clean install
default: demo

demo:
	@echo "--- Demo ---"
	ocamlfind opt -o example \
          -pp cppo-json \
          -package atdgen -linkpkg \
          example.ml
	./example

install:
	cp atdgen-cppo cppo-json "$(BINDIR)"

clean:
	rm -f *.cm[iox] *.o *~ example
