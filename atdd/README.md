Atdd
==

Atdd takes type definitions in the ATD format and derives `dlang`
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, Python or Scala.

See the sample input type definitions
[everything.atd](test/atd-input/everything.atd) and
the D output [everything.d](test/dlang-expected/everything.d).

Requirements
--

Requirements for building and testing `atdd`:
* Opam and dependencies installed from the [`atd` project root](..)
  with `make setup`.
* ldc >= 1.27.0

Requirements for generating D code:
* the `atdd` executable

Requirements for compiling the generated D code:
* ldc >= 1.27.0

Documentation
--

* TODO

Development notes
--

Build or rebuild with `make`. Test with `make test`. This requires
ldc2.

Running the tests is done from the `atdd/` main folder with `make
test`.

We have two kinds of tests for atdpy:
* [unit tests](src/test) for testing internal OCaml code
* code generation and D tests:
  * they generate D code from ATD files and compare the D output
    against the [expectations](dlang-expected).
  * the generated code is executed by some tests.
