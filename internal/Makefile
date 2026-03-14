# Feature Support Matrix Generator
# ==================================
#
# This directory contains a small OCaml program (src/main.ml) that generates
# the feature support matrix page for the ATD documentation
# (../doc/support-matrix.rst).
#
# Editing the table
# -----------------
# All data lives in src/main.ml as typed OCaml values (yes/no/planned per
# feature per backend). This makes it easy to update individual cells, add
# rows or columns, and keep the RST output consistently formatted — much
# easier than editing a wide hand-crafted RST table directly.
#
# Regenerating the RST file
# -------------------------
#   make          build the program and overwrite ../doc/support-matrix.rst
#   make build    build only, do not overwrite the RST file
#
# The generated file is committed to the repository so that the documentation
# can be built without running OCaml.

DUNE := dune
ROOT := ..
OUT  := $(ROOT)/doc/support-matrix.rst

.PHONY: all build

all: build
	cd $(ROOT) && $(DUNE) exec internal/support_matrix.exe > doc/support-matrix.rst

build:
	cd $(ROOT) && $(DUNE) build internal/support_matrix.exe
