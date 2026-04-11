# Build and run the atd-yamlx example.
#
# Prerequisites (all available via opam):
#   atdml        opam install atdml
#   atd-jsonlike opam install atd-jsonlike
#   atd-yamlx    opam install atd-yamlx
#   yamlx        opam install yamlx
#   yojson       opam install yojson
#
# Usage:
#   make          -- generate, compile, and run
#   make generate -- regenerate app_config.ml / app_config.mli from the ATD file
#   make build    -- compile the example binary
#   make run      -- run it against config.yaml
#   make clean    -- remove generated and compiled files

OCAMLFIND  = ocamlfind
OCAMLOPT   = ocamlopt
ATDML      = atdml
PACKAGES   = yojson,atd-jsonlike,yamlx,atd-yamlx

GENERATED  = app_config.ml app_config.mli
BINARY     = main

.PHONY: default generate build run clean

default: generate build run

generate: $(GENERATED)

$(GENERATED): app_config.atd
	$(ATDML) app_config.atd

build: $(BINARY)

$(BINARY): $(GENERATED) main.ml
	$(OCAMLFIND) $(OCAMLOPT) -package $(PACKAGES) -linkpkg \
		app_config.mli app_config.ml main.ml -o $(BINARY)

run: $(BINARY)
	./$(BINARY)

clean:
	rm -f $(GENERATED) $(BINARY) *.cmi *.cmo *.cmx *.o
