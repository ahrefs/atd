atdcpp
==

atdcpp takes type definitions in the ATD format and derives `C++`
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, Python or Scala.

See the sample input type definitions
[everything.atd](test/atd-input/everything.atd) and
the C++ output [everything.hpp](test/cpp-expected/everything.hpp).

This implementation makes use of the RapidJson C++ library.

Requirements
--

Requirements for building and testing `atdcpp`:
* Opam and dependencies installed from the [`atd` project root](..)
  with `make setup`.
* gcc / clang
* librapidjson

Requirements for generating C++ code:
* the `atdcpp` executable

Requirements for compiling the generated C++ code:
* A working C++ compiler (gcc / clang)
* The rapidjson library

Documentation
--

* TODO

Development notes
--

Build or rebuild with `make`. Test with `make test`.

Running the tests is done from the `atdcpp/` main folder with `make
test`.

We have two kinds of tests for atdcpp:
* code generation and C++ tests:
  * they generate C++ code from ATD files and compare the C++ output
    against the [expectations](cpp-expected).
  * the generated code is executed by some tests.
