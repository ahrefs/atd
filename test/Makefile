.PHONY: default test really-test
default: test

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
  EXE=.exe
else
  EXE=
endif

ATDGEN = ../src/atdgen$(EXE)

test:
	$(MAKE) -C ../src
	OCAMLPATH=..:$$OCAMLPATH $(MAKE) really-test

really-test:
	$(ATDGEN) test.atd
	$(ATDGEN) test2.atd
	$(ATDGEN) -json -extend Test \
	  -j-custom-fields \
 'fun loc s -> Printf.printf "Warning: skipping field %s (def: %s)\n" s loc' \
	  test.atd -o testj
	$(ATDGEN) -std-json -extend Test test.atd -o testjstd
	$(ATDGEN) -json -extend Test2 test2.atd -o test2j
	$(ATDGEN) -t test3j.atd
	$(ATDGEN) -j -j-std -j-strict-constrs test3j.atd
	$(ATDGEN) test4.atd
	$(ATDGEN) -json test4.atd -o test4j
	$(ATDGEN) -validate -extend Test test.atd -o testv
	$(ATDGEN) -o-name-overlap -t test5.atd
	$(ATDGEN) -o-name-overlap -j test5.atd
	$(ATDGEN) -o-name-overlap -b test5.atd
	$(ATDGEN) -t test_type_conv.atd -type-conv sexp -open "Sexplib.Std"
	ocamlfind ocamlc -c -package atdgen \
		test5_t.mli test5_t.ml test5_j.mli test5_j.ml
	ocamlfind ocamlc -c -package atdgen \
		test5_t.mli test5_t.ml test5_b.mli test5_b.ml
	ocamlfind ocamlopt -c -g test_lib.ml
	ocamlfind ocamlc -c -g test.mli -package atdgen
	ocamlfind ocamlopt -c -g test.ml -package atdgen
	ocamlfind ocamlc -c -g test2.mli -package atdgen
	ocamlfind ocamlopt -c -g test2.ml -package atdgen
	ocamlfind ocamlc -c -g test3j_t.mli test3j_j.mli -package atdgen
	ocamlfind ocamlopt -c -g test3j_t.ml test3j_j.ml -package atdgen
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
		test2j.cmx test3j_t.cmx test3j_j.cmx \
		testv.cmx test_atdgen_main.cmx
	mkdir -p testdoc
	ocamlfind ocamldoc -html -d testdoc -package atdgen \
		test.mli test2.mli testj.mli test2j.mli \
		test3j_t.mli test4.mli test4j.mli testv.mli
	./test_atdgen

.PHONY: test-all
test-all: really-test
	ocamlfind ocamlc -c -g -syntax camlp4o -package camlp4 \
	  -package sexplib -package sexplib.syntax \
	  test_type_conv_t.mli
	ocamlfind ocamlopt -c -g -syntax camlp4o -package camlp4 \
	  -package sexplib -package sexplib.syntax \
	  test_type_conv_t.ml
	ocamlfind ocamlopt -c -g test_atdgen_type_conv.ml -package atdgen -package sexplib
	ocamlfind ocamlopt -o test_atdgen_type_conv$(EXE) -g -linkpkg \
	  -syntax camlp4o -package camlp4 \
	  -package sexplib -package sexplib.syntax \
	  test_type_conv_t.cmx test_atdgen_type_conv.cmx
	./test_atdgen_type_conv

# Benchmarking and more testing

.PHONY: bench
bench: opt
	ocamlfind ocamlopt -c -g test_lib.ml -package atdgen
# biniou
	$(ATDGEN) test.atd
	$(ATDGEN) -open Test test2.atd
	ocamlfind ocamlc -c -g test.mli -package atdgen
	ocamlfind ocamlopt -c -g test.ml -package atdgen
	ocamlfind ocamlc -c -g test2.mli -package atdgen
	ocamlfind ocamlopt -c -g test2.ml -package atdgen
# json
	$(ATDGEN) -json -std-json -o testj -open Test -ntd test.atd
	$(ATDGEN) -json -std-json -o test2j -open Test,Test2,Testj -ntd \
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
		benchmark test_atdgen \
		gmon.out ocamlprof.dump \
		test.bin test-2.bin test.json test-2.json \
		test-std.json test-json-files.json test-json-streams.json \
		test.ml test.mli testj.ml testj.mli \
		test2.ml test2.mli test2j.ml test2j.mli \
		test3j.mli test3j.ml \
		test4.mli test4.ml test4j.mli test4j.ml \
		test5_b.mli test5_b.ml test5_j.mli test5_j.ml \
		test5_t.mli test5_t.ml testjstd.ml testjstd.mli \
		testv.ml testv.mli
	rm -rf testdoc
