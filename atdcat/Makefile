DUNE ?= dune

.PHONY: build
build:
	$(DUNE) build

# Run all tests.
# Snapshots are stored in tests/named-snapshots/.
# Use 'make test-approve' to accept new or changed output.
.PHONY: test
test: build
	$(DUNE) build tests/test.exe
	./test

# Approve new or changed snapshots produced by 'make test'.
.PHONY: test-approve
test-approve: build
	$(DUNE) build tests/test.exe
	./test approve

# Show the expected JSON Schema in YAML format (for visual inspection)
.PHONY: yaml
yaml:
	yq e -P tests/schema.expected.json
