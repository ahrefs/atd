Atdj
====

Atdj is a program that generates a Java interface from type definitions.
In particular, given a set of ATD type definitions,
this tool generates a set of Java classes representing those types
with built-in JSON serializers and deserializers.

The primary benefits of using the generated interface, over manually
manipulating JSON strings from within Java, are safety and ease of use.
Specifically, the generated interface offers the following features:

* JSON strings are automatically checked for correctness with
  respect to the ATD specification.
* Details such as optional fields and their associated default values are
  automatically handled.

Installation
============

Building atdj requires atd and omake.
The easiest way to set this up is with opam
(http://opam.ocamlpro.com/).
Once opam is installed, run:
```
opam install atd
opam install omake
```

Atdj is then built and installed as follows:
```
make
make install
```

By default `atdj` is installed into `$HOME/bin`, but you can change this
location as follows:
```
make BINDIR=/usr/local/bin install
```
or
```
make PREFIX=/usr/local install
```

Getting started
===============

Look into `test.atd`, run `make test`, and look into the generated
code under `com/mylife/test`.

TODO: document the supported features.
