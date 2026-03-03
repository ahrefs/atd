Atdml
==

Atdml takes type definitions in the [ATD format](https://atd.readthedocs.io)
and generates an OCaml module that serializes and deserializes values to and
from JSON. For each ATD type `foo`, the generated code provides:

- A type definition `type foo = ...`
- A creation function `val make_foo : ... -> foo` (for record types)
- Conversion functions `val foo_of_yojson : Yojson.Safe.t -> foo` and
  `val yojson_of_foo : foo -> Yojson.Safe.t`
- Convenience I/O functions `val foo_of_string`, `val string_of_foo`, etc.

One ATD file `foo.atd` produces one OCaml module `foo.ml` + `foo.mli`.

Why atdml exists
--

Atdml is a simpler alternative to [atdgen](../atdgen) for the JSON use case.
Atdgen's JSON backend generates highly optimized OCaml code that bypasses the
Yojson AST and calls internal streaming parser functions directly. This
approach is fast but makes the generated code difficult to read, and the code
generator itself difficult to maintain and extend.

Atdml takes a different approach: it uses `Yojson.Safe.t` as the intermediate
representation throughout, so the generated code is straightforward to
understand and the generator is much simpler to work with.

The trade-off is performance. Using an intermediate JSON AST instead of
streaming directly into OCaml values is expected to be noticeably slower —
benchmarks have not been run yet, but a slowdown of 4× or more relative to
atdgen is plausible for parsing-heavy workloads (?).

How atdml differs from atdgen
--

| | atdgen | atdml |
|---|---|---|
| Output files | `foo_t.ml` + `foo_j.ml` (+ `_b`, `_v`) | `foo.ml` + `foo.mli` |
| JSON intermediate | Streaming (no AST) | `Yojson.Safe.t` |
| Default variants | Polymorphic (`` `Foo ``) | Classic (`Foo`) |
| Creation functions | Optional (`-create-mandatory`) | Always generated |
| Naming convention | `read_foo` / `write_foo` | `foo_of_yojson` / `yojson_of_foo` |
| PPX naming compat. | No | Yes (`ppx_yojson_conv`) |
| Runtime library | `atdgen-runtime` | `atdml-runtime` |
| Performance | Optimized | Simpler but slower (?) |
| Biniou support | Yes | No |

Polymorphic variants can be opted into per sum type with
`<ocaml repr="poly">`. Default field values are inferred from the type for
common cases (`list` → `[]`, `option` → `None`, `bool` → `false`, etc.) or
specified explicitly with `<ml default="...">`.

Who should use atdml
--

Consider atdml if:

- You are writing a new OCaml project and want clean, readable generated code
  that is easy to debug.
- JSON parsing performance is not a bottleneck in your application.
- You want generated code that is compatible with the `ppx_yojson_conv`
  naming convention, so it can coexist with PPX-derived types.
- You want a single output module per ATD file instead of the `_t`/`_j` split.

Stick with atdgen if:

- You have an existing codebase that already uses atdgen and migration is not
  worthwhile.
- You need Biniou support.
- JSON parsing performance is critical.

Status and reliability
--

**Atdml is experimental.** It was initially created with assistance from
[Claude Code](https://claude.ai/claude-code) and has not yet been
battle-tested in production. Expect rough edges, missing features, and
possible bugs. Contributions and bug reports are very welcome.

Features not yet implemented:

- `wrap` construct for custom type conversions
- `shared` construct
- JSON adapters (`<json adapter.ocaml=...>`)
- Open enums (`<json open_enum>`)
- Biniou format (not planned)
- `abstract` types beyond the identity case

Requirements
--

To build and run atdml:

- OCaml >= 4.08
- `atd` library
- `atdml-runtime` (for the generated code)
- `yojson` >= 2.0.0

Install via opam (tbd — atdml is not yet published to opam):

```
opam install atdml
```

Usage
--

```
atdml foo.atd
```

This produces `foo.ml` and `foo.mli` in the current directory. Add
`atdml-runtime` and `yojson` as dependencies in your project's `dune` file:

```
(library
 (name mylib)
 (libraries atdml-runtime yojson)
)
```

Documentation
--

- Main ATD documentation: https://atd.readthedocs.io (tbd: atdml-specific page)
- Command-line help: `atdml --help`
- GitHub issue tracking the design: https://github.com/ahrefs/atd/issues/313

Contributing
--

Bug reports and pull requests are welcome at
https://github.com/ahrefs/atd/issues. Please tag issues with `target:ocaml`
if applicable.
