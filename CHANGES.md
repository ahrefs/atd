1.13.0 (2018-03-27)
-------------------

* Introduce `atdgen-runtime` package. This package contains the runtime
  dependency introduced by the `atdgen` code generator. The old runtime
  library is deprecated

* Add `atdj` to set of released packages. `atdj` is a java code generator
  for .atd files.

* Improve generated code to emit ppx attributes to ignore harmless warnings

* `Ag_version` submodule has been replaced with `Version`.

* Transition `atd` aliases using the `(wrapped true)` mode of
  jbuilder. This is a breaking change for all of those who use `atd`
  the library. All modules are now accessible under the `Atd.` entry module.
