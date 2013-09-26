VERSION = 1.2.5
ifeq "$(shell ocamlc -config |grep os_type)" "os_type: Win32"
EXE=.exe
else
EXE=
endif

# Shared stuff
SOURCES_SHARED = \
  ag_version.ml \
  ag_error.ml \
  ag_mapping.ml \
  ag_doc_lexer.mll \
  ag_doc.mli ag_doc.ml \
  ag_ocaml.ml \
  ag_indent.ml \
  ag_ox_emit.ml

# Biniou/OCaml
SOURCES_BINIOU = \
  ag_biniou.ml \
  ag_xb_emit.ml \
  ag_ob_mapping.ml \
  ag_ob_spe.ml \
  ag_ob_emit.ml

# JSON/OCaml
SOURCES_JSON = \
  ag_string_match.mli ag_string_match.ml \
  ag_json.ml \
  ag_oj_mapping.ml \
  ag_oj_emit.ml

# OCaml validators
SOURCES_VALIDATE = \
  ag_validate.ml \
  ag_ov_mapping.ml \
  ag_ov_emit.ml

# OCaml runtime library
SOURCES_RUNTIME = \
  ag_ob_run.ml \
  ag_oj_run.ml \
  ag_ov_run.ml \
  ag_util.mli ag_util.ml

SOURCES = \
  $(SOURCES_SHARED) $(SOURCES_BINIOU) $(SOURCES_JSON) $(SOURCES_VALIDATE) \
  $(SOURCES_RUNTIME)

DOCFILES = ag_doc ag_util
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
CMXS = $(patsubst %.ml,%.cmxs, $(ML))
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

.PHONY: all opt install uninstall reinstall
all: pp
	$(MAKE) atdgen.cma atdgen.run
opt: pp
	$(MAKE) atdgen.cmxa atdgen.cmxs atdgen

install: META
	test ! -f atdgen.run || cp atdgen.run $(BINDIR)/
	test ! -f atdgen.run.exe || cp atdgen.run.exe $(BINDIR)/
	test ! -f atdgen || cp atdgen $(BINDIR)/
	test ! -f atdgen.exe || cp atdgen.exe $(BINDIR)/
	ocamlfind install atdgen META \
		 $(MLI) $(CMI) $(CMO) $(CMX) $(CMXS) $(O) \
			atdgen.cma atdgen.a atdgen.cmxa

uninstall:
	test ! -f $(BINDIR)/atdgen.run || rm $(BINDIR)/atdgen.run
	test ! -f $(BINDIR)/atdgen.run.exe || rm $(BINDIR)/atdgen.run.exe
	test ! -f $(BINDIR)/atdgen || rm $(BINDIR)/atdgen
	test ! -f $(BINDIR)/atdgen.exe || rm $(BINDIR)/atdgen.exe
	ocamlfind remove atdgen

reinstall:
	$(MAKE) uninstall || :
	$(MAKE) install

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

atdgen.cmxs: dep $(CMI) $(CMX)
	ocamlfind ocamlopt $(OCAMLFLAGS) -shared -o atdgen.cmxs $(CMX)

atdgen.run: dep $(CMI) $(CMO) ag_main.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -o atdgen.run \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMO) ag_main.ml

atdgen$(EXE): dep $(CMI) $(CMX) ag_main.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atdgen$(EXE) \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMX) ag_main.ml



.PHONY: doc
doc: odoc/index.html atdgen$(EXE)
	cd manual; $(MAKE)

odoc/index.html: $(CMI)
	mkdir -p odoc
	ocamlfind ocamldoc -d odoc -html \
		-t 'Atdgen library documentation' \
		-package "$(OCAMLPACKS)" $(DOCSOURCES)

# Some testing

.PHONY: test really-test
test: opt
	OCAMLPATH=..:$$OCAMLPATH $(MAKE) really-test

really-test:
	./atdgen test.atd
	./atdgen test2.atd
	./atdgen -json -extend Test \
	  -j-custom-fields \
 'fun loc s -> Printf.printf "Warning: skipping field %s (def: %s)\n" s loc' \
	  test.atd -o testj
	./atdgen -std-json -extend Test test.atd -o testjstd
	./atdgen -json -extend Test2 test2.atd -o test2j
	./atdgen test3b.atd
	./atdgen -json test3j.atd
	./atdgen test4.atd
	./atdgen -json test4.atd -o test4j
	./atdgen -validate -extend Test test.atd -o testv
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
	ocamlfind ocamlc -c -g testv.mli -package atdgen
	ocamlfind ocamlopt -c -g testv.ml -package atdgen
	ocamlfind ocamlopt -c -g test_atdgen_main.ml -package atdgen
	ocamlfind ocamlopt -o test_atdgen$(EXE) -g -linkpkg -package atdgen \
		test_lib.cmx test.cmx test2.cmx testj.cmx testjstd.cmx \
		test2j.cmx test3b.cmx test3j.cmx testv.cmx test_atdgen_main.cmx
	mkdir -p testdoc
	ocamlfind ocamldoc -html -d testdoc -package atdgen \
		test.mli test2.mli testj.mli test2j.mli test3b.mli \
		test3j.mli test4.mli test4j.mli testv.mli
	./test_atdgen


# Benchmarking and more testing

.PHONY: bench
bench: opt
	ocamlfind ocamlopt -c -g test_lib.ml -package atdgen
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
		dep atdgen$(EXE) atdgen.run \
		benchmark test_atdgen \
		gmon.out ocamlprof.dump \
		test.bin test-2.bin test.json test-2.json \
		test-std.json test-json-files.json test-json-streams.json \
		test.ml test.mli testj.ml testj.mli \
		test2.ml test2.mli test2j.ml test2j.mli \
                test3b.mli test3b.ml \
		test3j.mli test3j.ml \
		test4.mli test4.ml test4j.mli test4j.ml \
		ag_doc_lexer.ml
	rm -rf odoc testdoc
	cd manual && $(MAKE) clean
