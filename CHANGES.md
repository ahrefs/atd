Unreleased
-------------------

* atddiff: Fix `atddiff --version` output (#379)
* atddiff: Add option `--no-locations` aimed at producing more stable results
           that allow diffing successive atddiff reports to spot new
           findings and ignore old ones.

2.14.1 (2023-10-20)
-------------------

* atddiff: Fixed reports for new variant cases. They are now correctly 
  reported as forward incompatibilities (#373)

2.14.0 (2023-10-19)
-------------------

* atdd: Fix various issues with the interoperability of user defined types,
  used in or outside of records (#355)
* atdd: Generated `.d` files now have the suffix `_atd.d` (#355)
* atddiff now supports options for filtering the findings based on the 
  direction of the incompatibility (`--backward`, `--forward`) or based on the  
  name of the affected types (`--types`) (#365)
* atddiff: new option `--output-format json` for exporting the results to
  JSON (#360)

2.13.0 (2023-10-15)
-------------------

* atdts: Stop compiler errors on generated typescript (#348)
* atdts: Don't fail on `wrap` constructs (#353)
* atdcat: New option `-remove-wraps` which pretty-prints the type
  definitions without `wrap` constructs (#353)
* atdd: Add `dlang` backend to generate D code from ATD definitions (#349)
* new tool: atddiff. Compares two versions of an ATD file and reports
  possible incompatibilities in the JSON data. Atddiff ships as part of the 
  `atd` package together with `atdcat` (#352, #358)

2.12.0 (2023-05-12)
-------------------

* atdgen: Annotate generated code with types to disambiguate OCaml
  classic variants (#331)
* atdpy: Support the option type more correctly so that it follows
  ATD's convention for JSON encoding. This allows compatibility with
  JSON produced by other tools of the ATD suite. The Python type,
  however, is still a nullable (`Optional`) to make things simpler for
  Python programmers. This prevents distinguishing `["Some", "None"]`
  from `"None"` which both translate to `None` in Python. (#332)
* (BREAKING) atdgen: revert default encoding of int64 values as string (#330)
* atdgen: Support `<json repr="string">` for `int` values (#330)
* atdpy: Treat default field values as expressions to evaluate each time
  they're assigned to a field. This allows the use of mutable defaults such as
  lists (#339)

2.11.0 (2023-02-08)
-------------------

* atdpy: Support parametrized type definitions (#303)
* atdpy: Support options approximately by treating them as nullables (#320)
* atdts: Support parametrized type definitions (#303)
* atdts: Fix incorrect type for TypeScript readers/writers generated
         for type `abstract`.
* atdts: Fix incorrect type for TypeScript writers of optional fields.
         It was working only in special cases such as `foo?: int`.
* atdts: Eliminate the type alias `type Int = number` since it was
         more confusing than helpful. Occurrences of `Int` are replaced
         by `number /*int*/`.
* atdts: Disable all tslint and eslint rules in generated code so as
         to play well with all tslint and eslint configurations.

2.10.0 (2022-08-09)
-------------------

* atdgen: use Yojson 2.0 API (#299)
* atdpy: Support recursive definitions (#315)
* atdts: fix nullable object field writer (#312)

2.9.1 (2022-06-10)
------------------

* atdgen: update `abstract` type validation to accept all input by default (#301)
* atdts: fix reader for case type (#302)

2.8.0 (2022-06-06)
------------------

* atdgen: use odoc syntax to disambiguate clashing names (#296)
* atdpy: propagate decorators on sum types to all constructor classes (#298)

2.7.0 (2022-05-17)
------------------

* Add ability to specify JSON/OCaml adapter with the arbitrary code
  using `<json adapter.to_ocaml="..." adapter.from_ocaml="...">` (#184).
* atdcat: add option `-jsonschema-no-additional-properties` for JSON Schema
  output to specify that JSON objects may not have extra properties
  (#293, #294).
* atdcat: add `title` field to JSON Schema output containing the name
  of root type (#294).
* atdcat: add command-line option to choose the version of JSON Schema
  to target. Options are the latest version "Draft 2020-12" and the
  previous version "Draft 2019-09" (#294).
* ATD language: the `abstract` built-in can now be used like any
  other type to hold untyped data, if the implementation supports it.
  The supported targets so far are OCaml/JSON (atdgen), Python
  (atdpy), TypeScript (atdts), JSON Schema (atdcat) (#295).

2.6.0 (2022-05-03)
------------------

* atdcat: add option `-jsonschema` to translate from ATD to JSON
  Schema (#284)

2.5.0 (2022-04-23)
------------------

* atdpy: make `atdpy --version` print the version of atdpy itself
  rather than the version of the `atd` library (#270)
* atdpy: fix handling of `nullable` and improve error message on
         `option` types used without optional fields (#277)
* Add TypeScript backend called atdts (#274)

2.4.1 (2022-03-25)
------------------

* atdpy: don't apply the `@dataclass` decorator twice if explicitly
  added by the user via an ATD annotation such as
  `<python decorator="dataclass(frozen=True)">` (#267)

2.4.0 (2022-03-24)
------------------

* atdpy: allow custom imports and class decorators to be added to the
  generated Python code.

2.3.3 (2022-03-16)
------------------

* Prevent incorrect validation errors for annotations of the form
  `<ocaml field_prefix=...>` and a few others (#258)

2.3.2 (2022-03-11)
------------------

* Fix package dependencies (#257)

2.3.1 (2022-03-10)
------------------

* Ensure that atdgen reports its own version rather than the version
  of the atd library.
* Fix version constraint on cmdliner.

2.3.0 (2022-03-10)
------------------

* Allow single-quoted strings as an alternative to double-quoted
  strings in ATD files (#239)
* Add Python backend called atdpy (#235)
* Add detection of misplaced annotations and misspelled annotation
  field names for atdgen targets and atdpy (#204, #227)
* atdpy: Downcase Python output files (#251)
* atdpy: Disable flake8 checks on generated code via a special comment (#252)
* atdgen: Add support for ppx attributes on individual type
  definitions (#238)
* (BREAKING) atdgen: change encoding of int64 values to string (#231)
* other enhancement and fixes (see git log)

2.2.0 (2020-09-03)
------------------

* Add support for merging double annotations (`<ocaml from="ProtoA"><ocaml predef>`)
* Add tests for annotation merging and target-specific annotations

2.1.0 (2019-12-3)
-----------------

* Fix bug preventing generated code from compiling when using
  json adapters on recursive types.
* Improve automatic error messages shown in case of failed validation.
  Now include the validator's name or code.
* Add support for json adapters in the bucklescript backend. (#153)

2.0.0 (2018-05-31)
------------------

* Add support for json adapters in OCaml (`<json adapter.ocaml=...>`)
* Add support for json enums with a catch-all case (`<json open_enum>`)
* Remove `<json tag_field=...>` and `<json untyped>`

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
