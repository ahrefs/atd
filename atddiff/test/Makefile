# Run the tests.

DUNE ?= dune

# We generate the dune files from the list of *_old.atd files because
# dune doesn't support dynamic targets.
#
# How to add a test?
#
# 1. Identify the folder corresponding to the atddiff options to be used.
#    Look into the script 'generate-dune-rules' to find out which folder
#    corresponds to which options.
# 2. Create a pair of files ${name}_old.atd and ${name}_new.atd in that
#    folder.
# 3. Run 'make'.
# 4. If you're satisfied with the output, run 'dune promote' to make it the
#    new reference for future tests.
#
.PHONY: test
test:
	./generate-dune-rules
	$(DUNE) runtest -f
