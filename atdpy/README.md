Atdpy
==

Atdpy takes type definitions in the ATD format and derives Python
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, or Scala.

See the sample input type definitions
[everything.atd](test/atd-input/everything.atd) and
the Python output [everything.py](test/python-expected/everything.py).

Documentation
--

* [Main documentation for
  atdpy](https://atd.readthedocs.io/en/latest/atdpy.html)
* Command-line documentation: `atdpy --help`

Development notes
--

Build or rebuild with `make`. Test with `make test`. This requires
`pytest` which can be installed with

```
pip install pytest mypy
```

Contributing
--

Help is welcome and there are various ways to help:
* Add examples to the documentation
* File a GitHub issue to report a problem
* Pick an issue that [has the `target:python`
  label](https://github.com/ahrefs/atd/issues?q=is%3Aissue+is%3Aopen+label%3Atarget%3Apython)
  and implement a solution
