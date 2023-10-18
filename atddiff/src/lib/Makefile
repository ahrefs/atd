#
# Generate a .ml file containing just the type definitions derived from
# an ATD file.
#
# We don't generate code that exercises the atdgen runtime library
# due to circular dependencies but atddiff users can.
#

# This requires an external installation of the atdgen command.
.PHONY: types
types: Atddiff_output_t.ml

Atddiff_output_t.ml: Atddiff_output.atd
	atdgen -t Atddiff_output.atd
