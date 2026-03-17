DUNE ?= dune

.PHONY: build
build:
	$(DUNE) build

# Run all tests.
# Snapshots are stored in test/named-snapshots/.
# Use 'make test-approve' to accept new or changed output.
.PHONY: test
test: build
	$(DUNE) build test/test.exe
	$(DUNE) exec --no-build test/test.exe -- run --chdir $(CURDIR)

# Approve new or changed snapshots produced by 'make test'.
.PHONY: test-approve
test-approve: build
	$(DUNE) build test/test.exe
	$(DUNE) exec --no-build test/test.exe -- approve --chdir $(CURDIR)

# Show the expected JSON Schema in YAML format (for visual inspection)
.PHONY: yaml
yaml:
	yq e -P test/schema.expected.json
