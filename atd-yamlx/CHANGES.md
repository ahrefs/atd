unreleased
----------

* Initial release. Provides `Atd_yamlx.of_yamlx_value`, a function that
  translates a `YAMLx.value` (from the `yamlx` library) into an
  `Atd_jsonlike.AST.t`, preserving source locations so that ATD-generated
  reader functions can report precise file/line/column error messages.
