Atdpy
==

Atdpy takes type definitions in the ATD format and derives Python
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, or Scala.

Development notes
--

Build or rebuild with `make`. Test with `make test`. This requires
`pytest` which can be installed with

```
pip install pytest mypy
```

[please indicate a more idiomatic way of doing this; the author
doesn't know python well enough]
