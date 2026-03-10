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
| Naming convention | `foo_of_string` / `string_of_foo` | `foo_of_json` / `json_of_foo` |
| Submodules | No | Yes (`foo` = `Foo.t`) |
| PPX naming compat. | No | Yes (`ppx_yojson_conv`) |
| Runtime library | `atdgen-runtime` | Inlined in generated file |
| Performance | Optimized | Simpler but slower (?) |
| Biniou support | Yes | No |
| ATD module imports | No | Yes |

Polymorphic variants can be opted into per sum type with
`<ocaml repr="poly">`. Default field values are inferred from the type for
common cases (`list` → `[]`, `option` → `None`, `bool` → `false`, etc.) or
specified explicitly with `<ocaml default="...">`.

Who should use atdml
--

Consider atdml if:

- You are writing a new OCaml project and want clean, readable generated code
  that is easy to debug.
- You want generated code that is compatible with the `ppx_yojson_conv`
  naming convention, so it can coexist with PPX-derived types.
- You want a single output module per ATD file instead of the `_t`/`_j` split.

Stick with atdgen if:

- You need Biniou support.
- JSON parsing performance is already your bottleneck with atdgen.
- You don't need new features.

Status and reliability
--

**Atdml is experimental.** It was initially created with assistance from
[Claude Code](https://claude.ai/claude-code) and has not yet been
battle-tested in production. Expect rough edges, missing features, and
possible bugs. Contributions and bug reports are very welcome.

Features not yet implemented:

- Record field prefix (`<ocaml field_prefix="...">`)
- Open enums (`<json open_enum>`)
- Biniou format (not planned)
- `abstract` types beyond the identity case

See
[Issues](https://github.com/ahrefs/atd/issues?q=is%3Aissue%20state%3Aopen%20label%3Atarget%3Aocaml)
for all open issues related to atdgen and atdml.

Requirements
--

To build and run atdml:

- OCaml >= 4.08
- `atd` library
- `yojson` >= 2.0.0

Generated files have no dependency beyond `yojson`: the small runtime helpers
are inlined as a local `Atdml_runtime` submodule inside each generated `.ml`.

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
`yojson` as a dependency in your project's `dune` file:

```
(library
 (name mylib)
 (libraries yojson)
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
