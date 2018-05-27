ATD project
==

ATD stands for Adaptable Type Definitions. It is a syntax for defining
cross-language data types. It is used as input to generate efficient
and type-safe serializers, deserializers and validators. The current target
languages are OCaml and Java.

The following opam packages are provided by the atd project:

* atdgen: executable that generates OCaml code dealing with JSON and
  biniou data formats
* atdj: executable that generates Java code dealing with JSON
* atd: library for parsing atd files used by code generators

[Documentation](https://mjambon.github.io/atdgen-doc/)

Help wanted
--

The ATD suite of tools is developed and maintained by
volunteers&mdash;&users like you.
[Various issues](https://github.com/mjambon/atd/issues) are in need
of attention. If you'd like to contribute, please leave a comment on the
issue you're interested in, or create a new issue. Experienced
contributors will guide you as needed.

See our [contribution guidelines](https://github.com/mjambon/documents/blob/master/how-to-contribute.md).

Authors
--

The ATD language and atdgen were originally created at MyLife by
Martin Jambon in 2010. Atdj was created at MyLife by John
Billings in 2010. Multiple volunteers contributed to the project after that.
Other prominent contributors have included Jeff Meister, David
Sheets, and Rudi Grinberg.

We distribute the source code under the terms of a BSD license.
