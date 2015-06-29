VERSION = 1.7.1
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
CMXS=atdgen.cmxs
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
	$(MAKE) atdgen.cmxa $(CMXS) atdgen$(EXE)

.PHONY: exe-install exe-uninstall
exe-install:
	test ! -f atdgen.run || cp atdgen.run $(BINDIR)/
	test ! -f atdgen.run.exe || cp atdgen.run.exe $(BINDIR)/
	test ! -f atdgen || cp atdgen $(BINDIR)/
	test ! -f atdgen.exe || cp atdgen.exe $(BINDIR)/

exe-uninstall:
	test ! -f $(BINDIR)/atdgen.run || rm $(BINDIR)/atdgen.run
	test ! -f $(BINDIR)/atdgen.run.exe || rm $(BINDIR)/atdgen.run.exe
	test ! -f $(BINDIR)/atdgen || rm $(BINDIR)/atdgen
	test ! -f $(BINDIR)/atdgen.exe || rm $(BINDIR)/atdgen.exe

.PHONY: findlib-install findlib-uninstall
findlib-uninstall:
	ocamlfind remove atdgen

findlib-install: META
	ocamlfind install atdgen META \
		 $(MLI) $(CMI) $(CMO) $(CMX) $(CMXS) $(O) \
			atdgen.cma atdgen.a atdgen.cmxa

.PHONY: install uninstall reinstall
uninstall: findlib-uninstall exe-uninstall

install: findlib-install exe-install

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
	ocamlfind ocamlopt $(OCAMLFLAGS) -shared -o $(CMXS) $(CMX)

atdgen.run: dep $(CMI) $(CMO) ag_main.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -o atdgen.run \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMO) ag_main.ml

atdgen$(EXE): dep $(CMI) $(CMX) ag_main.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -o atdgen$(EXE) \
		-package "$(OCAMLPACKS)" -linkpkg \
		$(CMX) ag_main.ml

.PHONY: clean
clean:
	rm -f *.o *.a *.cm* *~ *.annot \
          dep atdgen atdgen.exe atdgen.run \
          ag_doc_lexer.ml ag_version.ml META VERSION
