VERSION = 1.2.0
ifeq "$(shell ocamlc -config |grep os_type)" "os_type: Win32"
EXE=.exe
else
EXE=
endif

NATDYNLINK := $(shell if [ -f `ocamlc -where`/dynlink.cmxa ]; then \
                        echo YES; \
                      else \
                        echo NO; \
                      fi)

ifeq "${NATDYNLINK}" "YES"
CMXS=atd.cmxs
endif

SOURCES = \
  atd_version.ml \
  atd_ast.mli atd_ast.ml \
  atd_annot.mli atd_annot.ml \
  atd_parser.mli atd_parser.mly \
  atd_lexer.mll \
  atd_doc_lexer.mll atd_doc.mli atd_doc.ml \
  atd_print.mli atd_print.ml \
  atd_predef.ml \
  atd_check.ml \
  atd_expand.mli atd_expand.ml \
  atd_inherit.mli atd_inherit.ml \
  atd_sort.ml \
  atd_util.mli atd_util.ml \
  atd_reflect.mli atd_reflect.ml \
  atd_indent.mli atd_indent.ml

MLY = $(filter %.mly, $(SOURCES))
MLL = $(filter %.mll, $(SOURCES))

MLSOURCES = $(patsubst %.mll,%.ml, $(patsubst %.mly,%.ml, $(SOURCES)))
MLI = $(filter %.mli, $(MLSOURCES))
ML = $(filter %.ml, $(MLSOURCES))
CMI = $(patsubst %.ml,%.cmi, $(ML))
CMO = $(patsubst %.ml,%.cmo, $(ML))
CMX = $(patsubst %.ml,%.cmx, $(ML))
O = $(patsubst %.ml,%.o, $(ML))
INSTALL_EXTRAS = atd_check.ml atd_doc_lexer.mll atd_doc_lexer.ml \
		 atd_lexer.mll atd_lexer.ml atd_predef.ml atd_version.ml

OCAMLFLAGS = -dtypes -g
OCAMLPACKS = easy-format unix str

DOCFILES = \
  atd_ast \
  atd_annot \
  atd_doc \
  atd_print \
  atd_expand \
  atd_inherit \
  atd_util \
  atd_reflect \
  atd_indent

DOCSOURCES = $(addsuffix .mli, $(DOCFILES))

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: default all opt install uninstall reinstall

default: all opt

all: VERSION META atd.cma

opt: VERSION META atd.cmxa $(CMXS) atdcat$(EXE)

install: META
	test ! -f atdcat || cp atdcat $(BINDIR)/
	test ! -f atdcat.exe || cp atdcat.exe $(BINDIR)/
	ocamlfind install atd META \
	 $(MLI) $(CMI) $(CMO) $(CMX) $(CMXS) $(O) atd.cma atd.a atd.cmxa \
         $(INSTALL_EXTRAS)

uninstall:
	test ! -f $(BINDIR)/atdcat || rm $(BINDIR)/atdcat
	test ! -f $(BINDIR)/atdcat.exe || rm $(BINDIR)/atdcat.exe
	ocamlfind remove atd

reinstall:
	$(MAKE) uninstall || :
	$(MAKE) install

atd_version.ml: Makefile
	echo 'let version = "$(VERSION)"' > atd_version.ml

META: META.in Makefile
	echo 'version = "$(VERSION)"' > META
	cat META.in >> META

VERSION: Makefile
	echo $(VERSION) > VERSION

%.cmi: %.mli
	ocamlfind ocamlc $(OCAMLFLAGS) -c -package "$(OCAMLPACKS)" $<

%.cmi: %.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -c -package "$(OCAMLPACKS)" $<

%.cmo: %.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -c -package "$(OCAMLPACKS)" $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -c -package "$(OCAMLPACKS)" $<

atd_parser.mli: atd_parser.mly
	menhir $<

atd_parser.ml: atd_parser.mly
	menhir $<

atd_lexer.ml: atd_lexer.mll
	ocamllex $<

atd_doc_lexer.ml: atd_doc_lexer.mll
	ocamllex $<

dep: $(SOURCES) Makefile
	ocamlfind ocamldep -package "$(OCAMLPACKS)" $(MLI) $(ML) > dep

ifneq ($(MAKECMDGOALS),clean)
-include dep
endif

atd.cma: dep $(CMI) $(CMO)
	ocamlfind ocamlc $(OCAMLFLAGS) -o atd.cma -a $(CMO)

atd.cmxa: dep $(CMI) $(CMX)
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atd.cmxa -a $(CMX)

atd.cmxs: dep $(CMI) $(CMX)
	ocamlfind ocamlopt $(OCAMLFLAGS) -shared -o $(CMXS) $(CMX)

atdcat$(EXE): dep $(CMI) $(CMX) atdcat.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atdcat$(EXE) \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMX) atdcat.ml

unit-tests$(EXE): dep $(CMI) $(CMX) unit_tests.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -o unit-tests$(EXE) \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMX) unit_tests.ml


.PHONY: doc
doc: odoc/index.html atdcat$(EXE)
	cd manual; $(MAKE)

odoc/index.html: $(CMI)
	mkdir -p odoc
	ocamlfind ocamldoc -d odoc -html \
		-t 'ATD library documentation' \
		-package "$(OCAMLPACKS)" $(DOCSOURCES)

.PHONY: test
test: atdcat$(EXE) unit-tests$(EXE) test.atd test2.atd
	./unit-tests$(EXE)
	./atdcat test.atd > test.out
	./atdcat test.out > test.out.out
	cmp test.out test.out.out
	./atdcat -x test2.atd > test2.out

.PHONY: docdemo
docdemo: atdcat$(EXE) test.atd
	./atdcat test.atd -html-doc -strip ocaml > test-out.atd
	caml2html -ext html:cat test-out.atd -nf
	sed -i -e 's!</style>!\
div.atd-doc { \
  border-left: solid #ccc 6px; \
  margin-bottom: 50px; \
  margin-left: 30px; \
  padding: 5px; \
} \
div.atd-doc p { \
  margin: 0px; \
  padding: 0px; \
} \
div.atd-doc pre { \
  margin-left: 40px; \
  margin-right: 0px; \
  margin-top: 10px; \
  margin-bottom: 10px; \
} \
</style>!' test-out.atd.html

.PHONY: clean
clean:
	rm -f dep
	rm -f atd_version.ml
	rm -f $(CMI) $(CMO) $(CMX) $(O) *.annot *.cma *.cmxa *.a
	rm -f $(patsubst %.mly,%.mli, $(MLY))
	rm -f $(patsubst %.mly,%.ml, $(MLY))
	rm -f $(patsubst %.mll,%.ml, $(MLL))
	rm -f atdcat.cm[ioxa] atdcat.o atdcat.cma atdcat.cmxa atdcat$(EXE)
	rm -f unit-tests$(EXE)
	rm -rf odoc
	cd manual; $(MAKE) clean

.PHONY: release
release:
	./release.sh
