ATD stands for Adaptable Type Definitions. It is a syntax for defining
cross-language data types. It is used as input to generate efficient
serializers, deserializers and validators. The current target languages are OCaml and Java.

The following opam packages are provided by the atd project:

* atdgen: executable that generates OCaml code dealing with JSON and biniou data formats
* atdj: executable that generates Java code dealing with JSON
* atd: library for parsing atd files used by code generators

[Documentation](https://mjambon.github.io/atdgen-doc/)

The ATD language and its OCaml library were originally designed and implemented
at MyLife by Martin Jambon. We distribute the source code under the
terms of a BSD license.

[Contribution guidelines](https://github.com/mjambon/documents/blob/master/how-to-contribute.md)
