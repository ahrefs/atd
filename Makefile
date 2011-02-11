# $Id: Makefile 53195 2011-01-28 23:38:12Z martin $

VERSION = 1.1.1

# Shared stuff
SOURCES_SHARED = \
  ag_version.ml \
  ag_error.ml \
  ag_mapping.ml \
  ag_doc_lexer.mll \
  ag_doc.mli ag_doc.ml \
  ag_ocaml.ml \
  ag_indent.ml \
  ag_ox_emit.ml \
  ag_biniou.ml \
  ag_xb_emit.ml

# Biniou/OCaml
SOURCES_BINIOU = \
  ag_ob_mapping.ml \
  ag_ob_spe.ml \
  ag_ob_emit.ml

# JSON/OCaml
SOURCES_OCAML = \
  ag_string_match.mli ag_string_match.ml \
  ag_json.ml \
  ag_oj_mapping.ml \
  ag_oj_emit.ml

# Runtime library
SOURCES_RUNTIME = \
  ag_ob_run.ml \
  ag_oj_run.ml

SOURCES = \
  $(SOURCES_SHARED) $(SOURCES_BINIOU) $(SOURCES_OCAML) $(SOURCES_RUNTIME) 

DOCFILES = ag_doc
DOCSOURCES = $(addsuffix .mli, $(DOCFILES))


MLY = $(filter %.mly, $(SOURCES))
MLL = $(filter %.mll, $(SOURCES))
OCAMLLEX_ML = $(patsubst %.mll,%.ml, $(MLL))
OCAMLYACC_MLI = $(patsubst %.mly,%.mli, $(MLY))
OCAMLYACC_ML = $(patsubst %.mly,%.ml, $(MLY))

MLSOURCES = $(patsubst %.mll,%.ml, $(patsubst %.mly,%.ml, $(SOURCES)))
MLI = $(filter %.mli, $(MLSOURCES))
ML = $(filter %.ml, $(MLSOURCES))
CMI = $(patsubst %.ml,%.cmi, $(ML))
CMO = $(patsubst %.ml,%.cmo, $(ML))
CMX = $(patsubst %.ml,%.cmx, $(ML))
O = $(patsubst %.ml,%.o, $(ML))

OCAMLFLAGS = -dtypes -g
OCAMLPACKS = str atd biniou yojson

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: default
default: all opt

.PHONY: pp
pp: VERSION META $(OCAMLLEX_ML) $(OCAMLYACC_MLI) $(OCAMLYACC_ML)
	$(MAKE) dep

.PHONY: all opt
all: pp
	$(MAKE) atdgen.cma atdgen.run
opt: pp
	$(MAKE) atdgen.cmxa atdgen

install: META
	test ! -f atdgen.run || cp atdgen.run $(BINDIR)/
	test ! -f atdgen.run.exe || cp atdgen.run.exe $(BINDIR)/
	test ! -f atdgen || cp atdgen $(BINDIR)/
	test ! -f atdgen.exe || cp atdgen.exe $(BINDIR)/
	ocamlfind install atdgen META \
		$$(find $(MLI) $(CMI) $(CMO) $(CMX) $(O) \
			atdgen.cma atdgen.a atdgen.cmxa)

uninstall:
	test ! -f $(BINDIR)/atdgen.run || rm $(BINDIR)/atdgen.run
	test ! -f $(BINDIR)/atdgen.run.exe || rm $(BINDIR)/atdgen.run.exe 
	test ! -f $(BINDIR)/atdgen || rm $(BINDIR)/atdgen
	test ! -f $(BINDIR)/atdgen.exe || rm $(BINDIR)/atdgen.exe 
	ocamlfind remove atdgen


ag_version.ml: Makefile
	echo 'let version = "$(VERSION)"' > ag_version.ml

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

ag_doc_lexer.ml: ag_doc_lexer.mll
	ocamllex $<

dep: $(SOURCES) Makefile
	ocamlfind ocamldep -package "$(OCAMLPACKS)" $(MLI) $(ML) > dep

ifneq ($(MAKECMDGOALS),clean)
-include dep
endif

atdgen.cma: dep $(CMI) $(CMO)
	ocamlfind ocamlc $(OCAMLFLAGS) -o atdgen.cma -a $(CMO)

atdgen.cmxa: dep $(CMI) $(CMX)
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atdgen.cmxa -a $(CMX)

atdgen.run: dep $(CMI) $(CMO) ag_main.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -o atdgen.run \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMO) ag_main.ml

atdgen: dep $(CMI) $(CMX) ag_main.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atdgen \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMX) ag_main.ml



.PHONY: doc
doc: odoc/index.html atdgen
	cd manual; $(MAKE)

odoc/index.html: $(CMI)
	mkdir -p odoc
	ocamlfind ocamldoc -d odoc -html \
		-t 'Atdgen library documentation' \
		-package "$(OCAMLPACKS)" $(DOCSOURCES)

# Some testing

.PHONY: test
test: opt
	./atdgen test.atd
	./atdgen test2.atd
	./atdgen -json -extend Test test.atd -o testj
	./atdgen -std-json -extend Test test.atd -o testjstd
	./atdgen -json -extend Test2 test2.atd -o test2j
	./atdgen test3b.atd
	./atdgen -json test3j.atd
	./atdgen test4.atd
	./atdgen -json test4.atd -o test4j
	ocamlfind ocamlopt -c -g test_lib.ml
	ocamlfind ocamlc -c -g test.mli -package atdgen
	ocamlfind ocamlopt -c -g test.ml -package atdgen
	ocamlfind ocamlc -c -g test2.mli -package atdgen
	ocamlfind ocamlopt -c -g test2.ml -package atdgen
	ocamlfind ocamlc -c -g test3b.mli -package atdgen
	ocamlfind ocamlopt -c -g test3b.ml -package atdgen
	ocamlfind ocamlc -c -g test3j.mli -package atdgen
	ocamlfind ocamlopt -c -g test3j.ml -package atdgen
	ocamlfind ocamlc -c -g test4.mli -package atdgen
	ocamlfind ocamlopt -c -g test4.ml -package atdgen
	ocamlfind ocamlc -c -g testj.mli -package atdgen
	ocamlfind ocamlopt -c -g testj.ml -package atdgen
	ocamlfind ocamlc -c -g testjstd.mli -package atdgen
	ocamlfind ocamlopt -c -g testjstd.ml -package atdgen
	ocamlfind ocamlc -c -g test2j.mli -package atdgen
	ocamlfind ocamlopt -c -g test2j.ml -package atdgen
	ocamlfind ocamlc -c -g test4j.mli -package atdgen
	ocamlfind ocamlopt -c -g test4j.ml -package atdgen
	ocamlfind ocamlopt -c -g test_atdgen_main.ml -package atdgen
	ocamlfind ocamlopt -o test_atdgen -g -linkpkg -package atdgen \
		test_lib.cmx test.cmx test2.cmx testj.cmx testjstd.cmx \
		test2j.cmx test3b.cmx test3j.cmx test_atdgen_main.cmx
	mkdir -p testdoc
	ocamlfind ocamldoc -html -d testdoc -package atdgen \
		test.mli test2.mli testj.mli test2j.mli test3b.mli \
		test3j.mli test4.mli test4j.mli
	./test_atdgen


# Benchmarking and more testing

.PHONY: bench
bench: opt
# biniou
	./atdgen test.atd
	./atdgen -open Test test2.atd
	ocamlfind ocamlc -c -g test.mli -package atdgen
	ocamlfind ocamlopt -c -g test.ml -package atdgen
	ocamlfind ocamlc -c -g test2.mli -package atdgen
	ocamlfind ocamlopt -c -g test2.ml -package atdgen
# json
	./atdgen -json -std-json -o testj -open Test -ntd test.atd
	./atdgen -json -std-json -o test2j -open Test,Test2,Testj -ntd \
		test2.atd
	ocamlfind ocamlopt -c -g test_lib.ml
	ocamlfind ocamlc -c -g testj.mli -package atdgen
	ocamlfind ocamlopt -c -g testj.ml -package atdgen
	ocamlfind ocamlc -c -g test2j.mli -package atdgen
	ocamlfind ocamlopt -c -g test2j.ml -package atdgen
# comparison
	ocamlfind ocamlopt -c -g -syntax camlp4o \
		-package json-static,atdgen,unix \
		benchmark.ml
	ocamlfind ocamlopt -o benchmark -g \
		test_lib.cmx test.cmx test2.cmx testj.cmx test2j.cmx \
		benchmark.cmx \
		-package atdgen,unix,json-wheel -linkpkg


.PHONY: clean
clean:
	rm -f *.o *.a *.cm* *~ *.annot \
		dep atdgen atdgen.run \
		benchmark test_atdgen \
		gmon.out ocamlprof.dump \
		test.bin test-2.bin test.json test-2.json \
		test.ml test.mli testj.ml testj.mli \
		test2.ml test2.mli test2j.ml test2j.mli \
                test3b.mli test3b.ml \
		test3j.mli test3j.ml \
		test4.mli test4.ml test4j.mli test4j.ml \
		ag_doc_lexer.ml
	rm -rf odoc testdoc


.PHONY: release
release:
	./release.sh
