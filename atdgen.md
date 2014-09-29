% atdgen(1) user manual
% Martin Jambon
% September 21, 2014

Name
====

atdgen - derive code from type definitions

Synopsis
========

atdgen **-t** [_infile_**.atd**] [_options_...]

atdgen **-j** **-j-std** [_infile_**.atd**] [_options_...]

atdgen **-b** [_infile_**.atd**] [_options_...]

atdgen **-v** [_infile_**.atd**] [_options_...]

atdgen [_mode_] [_options_...]

atdgen **-help**

Description
===========

Atdgen is a command-line program that takes as input type definitions
in the [ATD syntax](http://mjambon.com/atd) and produces OCaml
code suitable for data serialization and deserialization.

Two data formats are currently supported, these are
[JSON](http://json.org/) and [biniou](http://mjambon.com/biniou.html),
a binary format with extensibility properties similar to JSON.
Atdgen-json and Atdgen-biniou will refer to Atdgen used in one context
or the other.

Atdgen was designed with efficiency and durability in mind. Software
authors are encouraged to use Atdgen directly and to write
tools that may reuse part of Atdgen's source code.

Atdgen uses the following packages that were developed in conjunction
with Atdgen:

* `atd`: parser for the syntax of type definitions
* `biniou`: parser and printer for biniou, a binary
  extensible data format
* [`yojson`](http://mjambon.com/yojson.html):
  parser and printer for JSON, a widespread text-based data format


Command-line usage
========

Command-line help
----------

Call `atdgen -help` for the full list of available options.

Atdgen-json example
----------

```
$ atdgen -t example.atd
$ atdgen -j example.atd
```


Input file `example.atd`:

```ocaml
type profile = {
  id : string;
  email : string;
  ~email_validated : bool;
  name : string;
  ?real_name : string option;
  ~about_me : string list;
  ?gender : gender option;
  ?date_of_birth : date option;
}

type gender = [ Female | Male ]

type date = {
  year : int;
  month : int;
  day : int;
}
```

is used to produce
files `example_t.mli`,
`example_t.ml`,
`example_j.mli` and
`example_j.ml`.
This is `example_j.mli`:
...


Module `Example_t` (files `example_t.mli` and
`example_t.ml`) contains all OCaml type definitions that
can be used independently from Biniou or JSON.

For convenience, these definitions are also made available from the
`Example_j` module whose interface is shown above.
Any type name, record field name or variant constructor can be
referred to using either module. For example, the OCaml
expressions `((x : Example_t.date) : Example_j.date)`
and `x.Example_t.year = x.Example_j.year` are both valid.

Atdgen-biniou example
----------

```
$ atdgen -t example.atd
$ atdgen -b example.atd
```

Input file `example.atd`:

```ocaml
type profile = {
  id : string;
  email : string;
  ~email_validated : bool;
  name : string;
  ?real_name : string option;
  ~about_me : string list;
  ?gender : gender option;
  ?date_of_birth : date option;
}

type gender = [ Female | Male ]

type date = {
  year : int;
  month : int;
  day : int;
}
```

is used to produce files `example_t.mli`,
`example_t.ml`,
`example_b.mli` and
`example_b.ml`.

This is `example_b.mli`:
...

Module `Example_t` (files `example_t.mli` and
`example_t.ml`) contains all OCaml type definitions that
can be used independently from Biniou or JSON.

For convenience, these definitions are also made available from the
`Example_b` module whose interface is shown above.
Any type name, record field name or variant constructor can be
referred to using either module. For example, the OCaml
expressions `((x : Example_t.date) : Example_b.date)`
and `x.Example_t.year = x.Example_b.year` are both valid.


Validator example
----------

```
$ atdgen -t example.atd
$ atdgen -v example.atd
```

Input file `example.atd`:

```ocaml
type month = int <ocaml validator="fun x -> x >= 1 && x <= 12">
type day = int <ocaml validator="fun x -> x >= 1 && x <= 31">

type date = {
  year : int;
  month : month;
  day : day;
}
  <ocaml validator="Date_util.validate_date">
```

is used to produce
files `example_t.mli`,
`example_t.ml`,
`example_v.mli` and
`example_v.ml`.
This is `example_v.ml`, showing how the user-specified
validators are used:

...



Default type mapping
========

The following table summarizes the default mapping between ATD types and
OCaml, biniou and JSON data types. For each language more
representations are available and are detailed in the next section of this
manual.

---------------------------------------------------------------------
ATD         OCaml               Biniou             JSON
----------- ------------------- ------------------ ------------------
`unit`      `unit`              unit               null

`bool`      `bool`              bool               boolean

`int`       `int`               svint              number (int)

`float`     `float`             float64            number (not int)

`string`    `string`            string             string

`option`    `option`            numeric variants
                                (tag 0)            None/Some variants

`list`      `list`              array              array

`shared`    no wrapping         no longer          not implemented
                                supported

variants    polymorphic         regular variants   variants
            variants            variants

record      record              record             object

tuple       tuple               tuple              array
---------------------------------------------------------------------

Notes:

* The JSON null value serves only as the unit value and is
  useful in practice only for instanciating parametrized types with
  "nothing". Option types have a distinct representation that does
  not use the null value.
* OCaml floats are written to JSON numbers with either a decimal
  point or an exponent such that they are distinguishable from
  ints, even though the JSON standard does not require a distinction
  between the two.
* The optional values of record fields denoted in ATD by a
  question mark are unwrapped or omitted in both biniou and JSON.
* JSON option values and JSON variants are represented in standard
  JSON (`atdgen -j -j-std`) by a single string e.g. `"None"`
  or a pair in which the
  first element is the name (constructor) e.g. `["Some", 1234]`.
  Yojson also provides a specific syntax for variants using edgy
  brackets: `<"None">`, `<"Some": 1234>`.
* Biniou field names and variant names other than the option types
  use the hash of the ATD field or variant name and cannot currently
  be overridden by annotations.
* JSON tuples in standard JSON (`atdgen -j -j-std`) use the
  array notation e.g.
  `["ABC", 123]`.
  Yojson also provides a specific syntax for tuples using parentheses,
  e.g. `("ABC", 123)`.
* Types defined as abstractare defined in
  another module.




ATD Annotations
========

Section '`json`'
-------

### Field '`name`' ###

Position: after field name or variant name

Values: any string making a valid JSON string value

Semantics: specifies an alternate object field name or variant
  name to be used by the JSON representation.

Example:

```ocaml
type color = [
    Black <json name="black">
  | White <json name="white">
  | Grey <json name="grey">
]

type profile = {
  id <json name="ID"> : int;
  username : string;
  background_color : color;
}
```

A valid JSON object of the `profile` type above is:
```
{
  "ID": 12345678,
  "username": "kimforever",
  "background_color": "black"
}
```


### Field '`repr`' ###

#### Association lists ####

Position: after `(string * _) list` type

Values: `object`

Semantics: uses JSON's object notation to represent association
lists.

Example:

```ocaml
type counts = (string * int) list <json repr="object">
```

A valid JSON object of the `counts` type above is:
```
{
  "bob": 3,
  "john": 1408,
  "mary": 450987,
  "peter": 93087
}
```
Without the annotation `<json repr="object">`, the data above
would be represented as:
```
[
  [ "bob", 3 ],
  [ "john", 1408 ],
  [ "mary", 450987 ],
  [ "peter", 93087 ]
]
```

#### Floats ####

Position: after `float` type

Values: `int`

Semantics: specifies a float value that must be rounded to the
nearest integer and represented in JSON without
a decimal point nor an exponent.

Example:

```ocaml
type unixtime = float <json repr="int">
```


Section '`biniou`'
-------

### Field '`repr`' ###

#### Integers ####

Position: after `int` type

Values: `svint` (default), `uvint`, `int8`,
`int16`, `int32`, `int64`

Semantics: specifies an alternate type for representing integers.
The default type is `svint`.
The other integers types provided by biniou are
supported by Atdgen-biniou.
They have to map to the corresponding OCaml types
in accordance with the following table:

------------------------------------------------------------------------
Biniou type  Supported OCaml type  OCaml value range
------------ --------------------- -------------------------------------
`svint`      `int`                 `min_int` ... `max_int`

`uvint`      `int`                 0 ... `max_int`, `min_int` ... -1

`int8`       `char`                `'\000'` ... `'\255'`

`int16`      `int`                 0 ... 65535

`int32`      `int32`               `Int32.min_int` ... `Int32.max_int`

`int64`      `int64`               `Int64.min_int` ... `Int64.max_int`
------------------------------------------------------------------------

In addition to the mapping above, if the OCaml type is `int`,
any biniou integer type can be read into OCaml data regardless of the
declared biniou type.

Example:

```ocaml
type t = {
  id : int
    <ocaml repr="int64">
    <biniou repr="int64">;
  data : string list;
}
```

#### Floating-point numbers ####

Position: after `float` type

Values: `float64` (default), `float32`

Semantics: `float32` allows for a shorter serialized representation
of floats, using 4 bytes instead of 8, with reduced precision.
OCaml floats always use 8 bytes, though.

Example:

```ocaml
type t = {
  lat : float <biniou repr="float32">;
  lon : float <biniou repr="float32">;
}
```


#### Arrays and tables ####

Position: applies to lists of records

Values: `array` (default), `table`

Semantics: `table` uses biniou's table format instead of a
regular array for serializing OCaml data into biniou.
Both formats are supported for reading into OCaml data
regardless of the annotation. The table format allows

Example:

```ocaml
type item = {
  id : int;
  data : string list;
}

type items = item list <biniou repr="table">
```


Section '`ocaml`'
-------

### Field '`predef`' ###

Position: left-hand side of a type definition, after the type name

Values: none, `true` or `false`

Semantics: this flag indicates that the corresponding OCaml
type definition must be omitted.

Example:

```ocaml
(* Some third-party OCaml code *)
type message = {
  from : string;
  subject : string;
  body : string;
}
```

```ocaml
(*
   Our own ATD file used for making message_of_string and
   string_of_message functions.
*)
type message <ocaml predef> = {
  from : string;
  subject : string;
  body : string;
}
```


### Field '`mutable`' ###

Position: after a record field name

Values: none, `true` or `false`

Semantics: this flag indicates that the corresponding OCaml
record field is mutable.

Example:

```ocaml
type counter = {
  total <ocaml mutable> : int;
  errors <ocaml mutable> : int;
}
```

translates to the following OCaml definition:

```ocaml
type counter = {
  mutable total : int;
  mutable errors : int;
}
```


### Field '`default`' ###


Position: after a record field name marked with a `\~{`}
symbol or at the beginning of a tuple field.


Values: any valid OCaml expression

Semantics: specifies an explicit default value for a field of an
OCaml record or tuple, allowing that field to be omitted.

Example:

```ocaml
type color = [ Black | White | Rgb of (int * int * int) ]

type ford_t = {
  year : int;
  ~color <ocaml default="`Black"> : color;
}

type point = (int * int * <ocaml default="0"> : int)
```


### Field '`from`' ###

Position: left-hand side of a type definition, after the type name

Values: OCaml module name without the `_t`, `_b`,
`_j` or `_v`
suffix. This can be also seen as the name of the original ATD file,
without the `.atd` extension and capitalized like an OCaml
module name.

Semantics: specifies the base name of the OCaml modules
where the type and values coming with that type are defined.

It is useful for ATD types defined as
`abstract` and for types annotated as predefined using
the annotation `<ocaml predef>`.
In both cases, the missing definitions must be provided by
modules composed of the base name and the standard suffix assumed by
Atdgen which is
`_t`, `_b`,
`_j` or `_v`.

Example:
First input file `part1.atd`:

```ocaml
type point = { x : int; y : int }
```

Second input file `part2.atd` depending on the first one:

```ocaml
type point <ocaml from="Part1"> = abstract
type points = point list
```


### Field '`module`' ###

In most cases since Atdgen 1.2.0
`module` annotations are deprecated in favor of `from`
annotations previously described.

Position: left-hand side of a type definition, after the type name

Values: OCaml module name

Semantics: specifies the OCaml module where the type and values
coming with that type are defined. It is useful for ATD types defined as
`abstract` and for types annotated as predefined using
the annotation `<ocaml predef>`.
In both cases, the missing definitions can be provided either by
globally opening an OCaml module with an OCaml directive or by specifying
locally the name of the module to use.

The latter approach is recommended because it allows to create
type and value aliases in the OCaml module being generated. It results
in a complete module signature regardless of the external
nature of some items.

Example:
Input file `example.atd`:

```ocaml
type document <ocaml module="Doc"> = abstract

type color <ocaml predef module="Color"> =
  [ Black | White ] <ocaml repr="classic">

type point <ocaml predef module="Point"> = {
  x : float;
  y : float;
}
```

gives the following OCaml type definitions
(file `example.mli`):

```ocaml
type document = Doc.document

type color = Color.color =  Black | White

type point = Point.point = { x: float; y: float }
```

Now for instance `Example.Black` and `Color.Black`
can be used interchangeably in other modules.


### Field '`t`' ###

Position: left-hand side of a type definition, after the type
name. Must be used in conjunction with a `module` field.

Values: OCaml type name as found in an external module.

Semantics: This option allows to specify the name of an
OCaml type defined in an external module.

It is useful when the type needs to be renamed because its original
name is already in use or not enough informative.
Typically we may want to give the name `foo` to a type
originally defined in OCaml as `Foo.t`.

Example:

```ocaml
type foo <ocaml_biniou module="Foo" t="t"> = abstract
type bar <ocaml_biniou module="Bar" t="t"> = abstract
type t <ocaml_biniou module="Baz"> = abstract
```

allows local type names to be unique
and gives the following OCaml type definitions:

```ocaml
type foo = Foo.t
type bar = Bar.t
type t = Baz.t
```



### Field '`field_prefix`' ###

Position: record type expression

Values: any string making a valid prefix for OCaml record field names

Semantics: specifies a prefix to be prepended to each field of
  the OCaml definition of the record.  Overridden by alternate field
  names defined on a per-field basis.

Example:

```ocaml
type point2 = {
  x : int;
  y : int;
} <ocaml field_prefix="p2_">
```

gives the following OCaml type definition:

```ocaml
type point2 = {
  p2_x : int;
  p2_y : int;
}
```

### Field '`name`' ###

Position: after record field name or variant name

Values: any string making a valid OCaml record field name or
  variant name

Semantics: specifies an alternate record field name or variant
  names to be used in OCaml.

Example:

```ocaml
type color = [
    Black <ocaml name="Grey0">
  | White <ocaml name="Grey100">
  | Grey <ocaml name="Grey50">
]

type profile = {
  id <ocaml name="profile_id"> : int;
  username : string;
}
```

gives the following OCaml type definitions:

```ocaml
type color = [
    `Grey0
  | `Grey100
  | `Grey50
]

type profile = {
  profile_id : int;
  username : string;
}
```


### Field '`repr`' ###

#### Integers ####

Position: after `int` type

Values: `char`, `int32`, `int64`, `float`

Semantics: specifies an alternate type for representing integers.
The default type is `int`, but `char`, `int32`,
`int64` or `float` can be used instead.

The three types `char`, `int32` and `int64` are
supported by both Atdgen-biniou and Atdgen-json but Atdgen-biniou
currently requires that they map to the corresponding fixed-width
types provided by the biniou format.

The type `float` is only supported in conjunction with JSON and
is useful when an OCaml float is used to represent an integral value,
such as a time in seconds returned by `Unix.time()`. When
converted into JSON, floats are rounded to the nearest integer.

Example:

```ocaml
type t = {
  id : int
    <ocaml repr="int64">
    <biniou repr="int64">;
  data : string list;
}
```


#### Lists and arrays ####

Position: after a `list` type

Values: `array`

Semantics: maps to OCaml's `array` type instead of `list`.

Example:

```ocaml
type t = {
  id : int;
  data : string list
    <ocaml repr="array">;
}
```

#### Sum types ####

Position: after a sum type (denoted by square brackets)

Values: `classic`

Semantics: maps to OCaml's classic variants instead of
polymorphic variants.

Example:

```ocaml
type fruit = [ Apple | Orange ] <ocaml repr="classic">
```

translates to the following OCaml type definition:

```ocaml
type fruit = Apple | Orange
```


#### Shared values ####

Position: after a `shared` type

This feature is obsolete and was last supported by atdgen 1.3.1.


### Field '`validator`' ###

Position: after any type expression except type variables

Values: OCaml function that takes one argument of the given type
and returns a bool

Semantics: `atdgen -v` produces for each type named
_t_ a function `validate_`_t_:

```ocaml
val validate_t : t -> bool
```

Such a function returns true if and only if the value and all of its
subnodes pass all the validators specified by annotations of the form
`<ocaml validator="...">`.

Example:

```ocaml
type positive = int <ocaml validator="fun x -> x > 0">

type point = {
  x : positive;
  y : positive;
  z : int;
}
  <ocaml validator="Point.validate">
  (* Some validating function from a user-defined module Point *)
```

The generated `validate_point` function is equivalent to the
following:

```ocaml
let validate_point p =
  Point.validate p
  && (fun x -> x > 0) p.x
  && (fun x -> x > 0) p.y
```


Section '`ocaml_biniou`'
-------

Section `ocaml_biniou` takes precedence over section `ocaml`
in Biniou mode (`-b`) for the following fields:

* `predef` (see section `ocaml`, field `predef`)
* `module` (see section `ocaml`, field `module`)
* `t` (see section `ocaml.t`)


Section '`ocaml_json`'
-------

Section `ocaml_json` takes precedence over section `ocaml`
in JSON mode (`-j`) for the following fields:

* `predef` (see section `ocaml`, field `predef`)
* `module` (see section `ocaml`, field `module`)
* `t` (see section `ocaml`, field `t`)


Example:

This example shows how to parse a field into a generic tree
of type `Yojson.Safe.json` rather than a value of a specialized
OCaml type.

```ocaml
type dyn <ocaml_json module="Yojson.Safe" t="json"> = abstract

type t = { foo: int; bar: dyn }
```

translates to the following OCaml type definitions:

```ocaml
type dyn = Yojson.Safe.json

type t = { foo : int; bar : dyn }
```

Sample OCaml value of type `t`:

```ocaml
{
  foo = 12345;
  bar =
    `List [
      `Int 12;
      `String "abc";
      `Assoc [
        "x", `Float 3.14;
        "y", `Float 0.0;
        "color", `List [ `Float 0.3; `Float 0.0; `Float 1.0 ]
      ]
    ]
}
```

Corresponding JSON data as obtained with `string_of_t`:
```
{"foo":12345,"bar":[12,"abc",{"x":3.14,"y":0.0,"color":[0.3,0.0,1.0]}]}
```


Section '`doc`'
-------

Unlike comments, `doc` annotations are meant to be
propagated into the generated source code.  This is useful for
making generated interface files readable without having to consult
the original ATD file.

Generated source code comments can comply to a standard format and
take advantage of documentation generators such as javadoc or
ocamldoc.


### Field '`text`' ###


Position:

* after the type name on the left-hand side of a type definition
* after the type expression on the right hand of a type definition
      (but not after any type expression)
* after record field names
* after variant names



Values: UTF-8-encoded text using a minimalistic markup language

Semantics: The markup language is defined as follows:

* Blank lines separate paragraphs.
* `{{ }}` can be used to enclose inline verbatim text.
* `{{{ }}}` can be used to enclose verbatim text where
  whitespace is preserved.
* The backslash character is used to escape special character sequences.
  In regular paragraph mode the special sequences are `\`, `{{` and `{{{`.
  In inline verbatim text, special sequences are `\` and `}}`.
  In verbatim text, special sequences are `\` and `}}}`.


Example: The following is a full example demonstrating the use of
`doc` annotations but also shows the full interface
file `genealogy.mli` generated using:

```
$ atdgen -b genealogy.atd
```


Input file `genealogy.atd`:

```ocaml
<doc text="Type definitions for family trees">

type tree = {
  members : person list;
  filiations : filiation list;
}

type filiation = {
  parent : person_id;
  child : person_id;
  filiation_type : filiation_type;
}
  <doc text="Connection between parent or primary caretaker and child">

type filiation_type = {
  ?genetic : bool option;
  ?pregnancy : bool option;
  ?raised_from_birth : bool option;
  ?raised : bool option;
  ?stepchild : bool option;
  ?adopted : bool option;
}
  <doc text="
Example of a father who raised his child from birth
but may not be the biological father:

{{{
{
  genetic = None;
  pregnancy = Some false;
  raised_from_birth = Some true;
  raised = Some true;
  stepchild = Some false;
  adopted = Some false;
}
}}}
">

type person_id
  <doc text="Two persons with the same {{person_id}} must be the same
             person. Two persons with different {{person_id}}s
             may be the same person if there is not enough evidence to
             support it."> = int

type person = {
  person_id : person_id;
  name : string;
  ~gender : gender list;
  ?biological_gender
    <doc text="Biological gender actually used for procreating"> :
    gender option;
}

type gender =
  [
  | F <doc text="female">
  | M <doc text="male">
  ]
    <doc text="Gender, definition depending on the context">
```

translates using `atdgen -b genealogy.atd`
into the following OCaml interface
file `genealogy_b.mli` with ocamldoc-compliant comments:
...




Library
========

A library named `atdgen` is installed by the standard
installation process. Only a fraction of it is officially supported
and documented. It is intended for tool developers. Please refer to
the comments in the [source code of atdgen](https://github.com/mjambon/atdgen).
