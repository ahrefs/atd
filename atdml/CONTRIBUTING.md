How to work with the atdml code base
==

Use `/atdml/` as your base directory.

For building the project, run `make`. This will make the `atdml`
binary available as a symlink.

To test the project, run `make test`. If any test fails, use the
`./test` command to review test results. It provides subcommands
to run, review, and approve test output.
Each subcommand has a `--help` option.
Consult [Testo's documentation](https://testocaml.net/) to get
started.

We rely on tests to validate each feature. Follow the model in
`tests/test.ml`.

A typical workflow is:

1. Set up the opam environment from the project root. `make setup`
   will take care of installing the opam dependencies.
2. cd into `/atdml/`.
3. Build the command-line executable with `make`.
4. Build and run the tests with `make test`.
5. Add a test for a new feature in `tests/` alongside the other
   similar tests. The tests are defined in an OCaml file and produce
   output that is compared against a reference kept under Git control.
   You'll find all of this in `tests/`.
6. Modify the OCaml code to get the feature you want. The source code
   lives in `src/`.
7. Rebuild and test with `make test`. This should also rebuild `atdml`
   since it is invoked by the tests.
8. Inspect each failed test individually using `./test status -s
   'TITLE OR ID'`.
9. Iterate until the tests work. If a test produces new output that
   requires approval, run `./test approve -s ...` to approve this
   output.

When you write code, please explain your intent and your choices so that
others don't have to guess what's intentional and what's a bug.

Pay attention to escaping and de-escaping names properly to avoid bugs
and security vulnerabilities.

For releases and other info pertaining to the whole atd project, see
[../CONTRIBUTING.md](../CONTRIBUTING.md).
