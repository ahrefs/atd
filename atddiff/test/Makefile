# Run the tests.

DUNE ?= dune

# We generate the dune file from the list of *_old.atd files because
# dune doesn't support dynamic targets.
# Each file "${name}_old.atd" file must be paired with "${name}_new.atd".
# They are the files to be compared by 'atddiff'.
.PHONY: test
test:
	./generate-dune-rules
	$(DUNE) runtest -f
