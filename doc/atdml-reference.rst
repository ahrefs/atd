================
Atdml reference
================

Description
-----------

Atdml is a command-line program that takes as input type definitions in the
`ATD syntax <atd-language.html>`__ and produces a self-contained OCaml module that serializes and
deserializes values to and from JSON via the
`Yojson <https://github.com/ocaml-community/yojson>`__ ``Safe.t``
intermediate representation.

Atdml is the recommended successor to atdgen for OCaml JSON support. Users
are encouraged to migrate from atdgen to atdml. Future development priority
will be given to atdml; atdgen will receive minimal maintenance only.

Compared to atdgen, atdml:

- generates a single ``foo.ml``/``foo.mli`` pair from a single ``foo.atd``
  file, with no need for a separate ``-t`` pass;
- requires only `yojson <https://opam.ocaml.org/packages/yojson/>`__ at
  runtime — there is no separate ``atdml-runtime`` package to link;
- generates straightforward, readable OCaml code that does not rely on
  internal Yojson streaming functions;
- does not support the biniou binary format (JSON only).

Atdml uses the following packages:

- ``atd``: parser for the syntax of type definitions
- `yojson <https://github.com/ocaml-community/yojson>`__: parser and
  printer for JSON

Command-line usage
------------------

Command-line help
^^^^^^^^^^^^^^^^^

Call ``atdml --help`` for the full list of available options.

File mode
^^^^^^^^^

::

    $ atdml foo.atd

Reads ``foo.atd`` and writes ``foo.ml`` and ``foo.mli`` in the current
directory. The base name is derived from the input file by stripping the
``.atd`` extension and lower-casing.

Stdin mode
^^^^^^^^^^

::

    $ atdml < foo.atd

Reads ATD source from stdin and writes a self-contained OCaml snippet to
stdout that can be copy-pasted into ``utop`` or ``ocaml``::

    module type Types = sig
      ...
    end

    module Types : Types = struct
      ...
    end

Example
^^^^^^^

Input file ``foo.atd``:

.. code:: ocaml

    type color = [
      | Red
      | Green
      | Blue
    ]

    type point = {
      x: float;
      y: float;
      ?label: string option;
    }

Generate ``foo.ml`` and ``foo.mli``::

    $ atdml foo.atd

The generated ``foo.mli`` exposes, for each ATD type, a type definition,
conversion functions, and a submodule:

.. code:: ocaml

    type color =
      | Red
      | Green
      | Blue

    val color_of_yojson : Yojson.Safe.t -> color
    val yojson_of_color : color -> Yojson.Safe.t
    val color_of_json : string -> color
    val json_of_color : color -> string

    module Color : sig
      type nonrec t = color
      val of_yojson : Yojson.Safe.t -> t
      val to_yojson : t -> Yojson.Safe.t
      val of_json : string -> t
      val to_json : t -> string
    end

    type point = {
      x: float;
      y: float;
      label: string option;
    }

    val make_point : x:float -> y:float -> ?label:string -> unit -> point
    val point_of_yojson : Yojson.Safe.t -> point
    val yojson_of_point : point -> Yojson.Safe.t
    val point_of_json : string -> point
    val json_of_point : point -> string

    module Point : sig
      type nonrec t = point
      val make : x:float -> y:float -> ?label:string -> unit -> t
      val of_yojson : Yojson.Safe.t -> t
      val to_yojson : t -> Yojson.Safe.t
      val of_json : string -> t
      val to_json : t -> string
    end

Generated interface
-------------------

For each ATD type ``foo``, atdml generates the following in the ``.mli``
and ``.ml`` files:

Type definition
^^^^^^^^^^^^^^^

A type definition matching the ATD definition, using the OCaml representations
described in the `Default type mapping`_ section below.

.. code:: ocaml

    type foo = ...

For record types, a ``make_foo`` constructor with labelled arguments is also
generated. Required fields use mandatory labelled arguments; optional fields
(``?foo``) and with-default fields (``~foo``) use optional labelled arguments:

.. code:: ocaml

    val make_foo : required_field:int -> ?optional_field:string -> unit -> foo

Conversion functions
^^^^^^^^^^^^^^^^^^^^

For each ATD type ``foo``:

.. code:: ocaml

    val foo_of_yojson : Yojson.Safe.t -> foo
    val yojson_of_foo : foo -> Yojson.Safe.t

    val foo_of_json : string -> foo
    val json_of_foo : foo -> string

For parametric types (e.g. ``type ('a, 'b) foo``), the conversion functions
take additional function arguments for the type parameters:

.. code:: ocaml

    val foo_of_yojson :
      (Yojson.Safe.t -> 'a) ->
      (Yojson.Safe.t -> 'b) ->
      Yojson.Safe.t -> ('a, 'b) foo

    val yojson_of_foo :
      ('a -> Yojson.Safe.t) ->
      ('b -> Yojson.Safe.t) ->
      ('a, 'b) foo -> Yojson.Safe.t

Submodule
^^^^^^^^^

Each type also gets a submodule named after the type, capitalised. The
submodule bundles the type and its conversion functions under a single name,
which is convenient for use as a module argument:

.. code:: ocaml

    module Foo : sig
      type nonrec t = foo
      val make : required_field:int -> ?optional_field:string -> unit -> t  (* record types only *)
      val of_yojson : Yojson.Safe.t -> t
      val to_yojson : t -> Yojson.Safe.t
      val of_json : string -> t
      val to_json : t -> string
    end

Reserved name handling
^^^^^^^^^^^^^^^^^^^^^^

Some ATD type names can conflict with OCaml keywords (``module``, ``type``,
…) or with the naming convention used by atdml (``yojson``, ``json``).
Atdml automatically renames such types by appending an underscore. For
example, an ATD type named ``module`` becomes ``module_`` in OCaml.

Default type mapping
--------------------

The following table summarizes the default mapping between ATD types and
OCaml types, and their JSON representations.

+--------------------+-----------------------+-----------------------------------------+
| ATD                | OCaml                 | JSON                                    |
+====================+=======================+=========================================+
| ``unit``           | ``unit``              | ``null``                                |
+--------------------+-----------------------+-----------------------------------------+
| ``bool``           | ``bool``              | ``true`` or ``false``                   |
+--------------------+-----------------------+-----------------------------------------+
| ``int``            | ``int``               | integer (e.g. ``42``)                   |
+--------------------+-----------------------+-----------------------------------------+
| ``float``          | ``float``             | number (e.g. ``3.14`` or ``42``)        |
+--------------------+-----------------------+-----------------------------------------+
| ``string``         | ``string``            | string (e.g. ``"hello"``)               |
+--------------------+-----------------------+-----------------------------------------+
| ``abstract``       | ``Yojson.Safe.t``     | any JSON value                          |
+--------------------+-----------------------+-----------------------------------------+
| ``'a list``        | ``'a list``           | JSON array                              |
+--------------------+-----------------------+-----------------------------------------+
| ``'a option``      | ``'a option``         | ``"None"`` or ``["Some", ...]``         |
+--------------------+-----------------------+-----------------------------------------+
| ``'a nullable``    | ``'a option``         | ``null`` or representation of ``'a``    |
+--------------------+-----------------------+-----------------------------------------+
| ``'a wrap``        | defined by annotation | representation of ``'a``                |
+--------------------+-----------------------+-----------------------------------------+
| ``('a * 'b)``      | ``('a * 'b)``         | JSON array (e.g. ``[1, "x"]``)          |
+--------------------+-----------------------+-----------------------------------------+
| sum type           | regular variants      | ``"Name"`` or ``["Name", ...]``         |
+--------------------+-----------------------+-----------------------------------------+
| sum type (poly)    | polymorphic variants  | ``"Name"`` or ``["Name", ...]``         |
+--------------------+-----------------------+-----------------------------------------+
| record             | record                | JSON object                             |
+--------------------+-----------------------+-----------------------------------------+

Notes:

- JSON integers are also accepted when a ``float`` is expected.
- ``'a option`` uses the ATD convention (not the ppx_yojson_conv one):
  ``"None"`` for ``None``, ``["Some", ...]`` for ``Some``.
- ``'a nullable`` maps to ``None`` for JSON ``null`` and ``Some x`` for any
  other value.
- The ``abstract`` type maps to ``Yojson.Safe.t``, accepting any JSON value.
- JSON tuples use the array notation, e.g. ``[1, "hello", true]``.
- Sum type constructors without arguments are represented as a JSON string,
  e.g. ``"Leaf"``. Constructors with an argument use a two-element array,
  e.g. ``["Node", ...]``.
- Optional record fields (marked with ``?``) are omitted from the JSON object
  when their value is ``None``.
- With-default record fields (marked with ``~``) are omitted from the JSON
  object when their value equals the default.

ATD Annotations
---------------

Section ``json``
^^^^^^^^^^^^^^^^

Field ``name``
""""""""""""""

Position: after a record field name or variant constructor name

Values: any string

Semantics: specifies an alternate name to use in the JSON representation,
overriding the ATD name.

Example:

.. code:: ocaml

    type color = [
      | Red
      | Green <json name="green">
      | Blue
    ]

    type profile = {
      id <json name="user_id">: string;
      name: string;
    }

A valid JSON object of the ``profile`` type above is:

.. code:: json

    {"user_id": "abc123", "name": "Alice"}

Section ``ocaml``
^^^^^^^^^^^^^^^^^

Field ``attr``
""""""""""""""

Position: on a type definition, i.e. on the left-hand side just before the
equal sign ``=``

Values: the contents of a ppx annotation without the enclosing ``[@@`` and
``]``

Semantics: appends a ppx attribute to the generated OCaml type definition,
in both the ``.mli`` and ``.ml`` files.

Example:

.. code:: ocaml

    type point <ocaml attr="deriving show, eq"> = {
      x: float;
      y: float;
    }

translates to

.. code:: ocaml

    type point = {
      x: float;
      y: float;
    }
    [@@deriving show, eq]

This is useful for attaching ppx rewriters such as ``ppx_deriving`` or
``ppx_yojson_conv`` to generated types. Note that the ppx library must be
present in the build environment; atdml does not provide it.

Field ``default``
"""""""""""""""""

Position: after a record field name marked with a ``~`` symbol

Values: any valid OCaml expression

Semantics: specifies an explicit default value for the field, overriding
the implicit default (``false`` for ``bool``, ``0`` for ``int``, ``""``
for ``string``, etc.). When reading JSON, the default is used when the
field is absent.

Example:

.. code:: ocaml

    type settings = {
      ~retries <ocaml default="3">: int;
      ~verbose: bool;
      ~label <ocaml default="\"unnamed\"">: string;
    }

Field ``name``
""""""""""""""

Position: after a variant constructor name

Values: any string making a valid OCaml constructor name

Semantics: specifies an alternate OCaml constructor name, overriding the
ATD name in the generated type definition. The JSON name is unaffected
(use ``<json name="...">`` to change the JSON name).

Example:

.. code:: ocaml

    type shape = [
      | Dot <ocaml name="Point">
      | Arc <json name="arc"> <ocaml name="ArcShape"> of float
    ]

translates to

.. code:: ocaml

    type shape =
      | Point
      | ArcShape of float

while the JSON representation still uses ``"Dot"`` and ``"arc"``.

Field ``repr``
""""""""""""""

Position: on a sum type expression (after the closing ``]``)

Values: ``"poly"``

Semantics: when ``repr="poly"``, the sum type is represented using OCaml
polymorphic variants (`` `Foo``) instead of regular variants (``Foo``).

Example:

.. code:: ocaml

    type status = [
      | Active
      | Inactive
      | Pending of string
    ] <ocaml repr="poly">

translates to

.. code:: ocaml

    type status = [
      | `Active
      | `Inactive
      | `Pending of string
    ]

The JSON representation is identical regardless of whether polymorphic or
regular variants are used.

Fields ``module``, ``t``, ``wrap``, and ``unwrap``
"""""""""""""""""""""""""""""""""""""""""""""""""""

Using a custom wrapper
~~~~~~~~~~~~~~~~~~~~~~

The built-in ``wrap`` type constructor allows adding a layer of abstraction
on top of the concrete type used for JSON serialization. The JSON
representation is unchanged; only the OCaml type is different.

Position: after a ``wrap`` type constructor

A common use case is to parse strings used as unique identifiers and
wrap the result into an abstract type. Given an OCaml module ``Uid`` that
provides:

.. code:: ocaml

    type t
    val wrap : string -> t
    val unwrap : t -> string

the following ATD definition:

.. code:: ocaml

    type uid = string wrap <ocaml module="Uid">

generates an OCaml type ``uid = Uid.t`` with serialization going through
``Uid.wrap`` and ``Uid.unwrap``.

It is also possible to specify ``t``, ``wrap``, and ``unwrap`` inline,
without a module:

.. code:: ocaml

    type uid = string wrap <ocaml t="Uid.t" wrap="Uid.wrap" unwrap="Uid.unwrap">

The ``module`` field can be combined with ``t`` to override the type name
while keeping the module's ``wrap``/``unwrap``:

.. code:: ocaml

    type uid = string wrap <ocaml module="Uid" t="Uid.uid">

Without any annotation, the ``wrap`` constructor has no effect on the OCaml
representation; ``wrap`` and ``unwrap`` both default to the identity function,
making the type a transparent alias for the inner type:

.. code:: ocaml

    (* No annotation: behaves like 'type tag = string' *)
    type tag = string wrap

The individual fields and their semantics:

- ``module``: an OCaml module name ``M``. Defaults for ``t``, ``wrap``, and
  ``unwrap`` become ``M.t``, ``M.wrap``, and ``M.unwrap``.
- ``t``: overrides the OCaml type name.
- ``wrap``: an OCaml expression used to convert from the serialized type to
  the abstract type (called on deserialization).
- ``unwrap``: an OCaml expression used to convert from the abstract type to
  the serialized type (called on serialization).

Optional and default record fields
-----------------------------------

ATD provides two ways to declare record fields that may be absent in JSON.

Optional fields (``?``)
^^^^^^^^^^^^^^^^^^^^^^^

A field prefixed with ``?`` is optional. In OCaml, its type is ``'a option``.
When missing from the JSON object, it deserializes to ``None``. When
serializing, a ``None`` value causes the field to be omitted from the JSON
object.

.. code:: ocaml

    (* ATD *)
    type person = {
      name: string;
      ?email: string option;
    }

The ``make_person`` function uses an OCaml optional argument for ``email``:

.. code:: ocaml

    val make_person : name:string -> ?email:string -> unit -> person

Note that ``?email:string`` (not ``?email:string option``) — OCaml wraps the
value in ``Some`` implicitly.

With-default fields (``~``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A field prefixed with ``~`` has a default value. When missing from the JSON
object, it deserializes to the default. When serializing, a value equal to the
default is omitted from the JSON object.

The default value is determined as follows:

- If an ``<ocaml default="EXPR">`` annotation is present, ``EXPR`` is used.
- Otherwise, the implicit default for the type is used: ``false`` for
  ``bool``, ``0`` for ``int``, ``0.`` for ``float``, ``""`` for ``string``,
  ``[]`` for lists.

.. code:: ocaml

    (* ATD *)
    type config = {
      ~timeout: int;
      ~verbose: bool;
      ~prefix <ocaml default="\"api_\"">: string;
    }

The ``make_config`` function uses OCaml optional arguments for all
with-default fields:

.. code:: ocaml

    val make_config : ?timeout:int -> ?verbose:bool -> ?prefix:string -> unit -> config

Mutually recursive types
------------------------

When two or more ATD types refer to each other, atdml detects the mutual
recursion and emits them as a ``type ... and ...`` group and ``let rec ...
and ...`` function groups. Types that are not mutually recursive are emitted
independently with plain ``type`` and ``let`` definitions, which keeps the
generated code easy to read and helps the OCaml compiler process each
definition efficiently.

Example:

.. code:: ocaml

    (* ATD *)
    type tree = [
      | Leaf
      | Node of node
    ]

    type node = {
      value: int;
      children: (tree * tree);
    }

These two types are mutually recursive, so atdml generates:

.. code:: ocaml

    type node = {
      value: int;
      children: (tree * tree);
    }

    and tree =
      | Leaf
      | Node of node

    let rec node_of_yojson ... = ...

    and tree_of_yojson ... = ...

Parametric types
----------------

ATD type parameters map directly to OCaml type parameters and to
higher-order function arguments in the conversion functions. Example:

.. code:: ocaml

    (* ATD *)
    type 'a result = [
      | Ok of 'a
      | Error of string
    ]

generates

.. code:: ocaml

    type 'a result =
      | Ok of 'a
      | Error of string

    val result_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a result
    val yojson_of_result : ('a -> Yojson.Safe.t) -> 'a result -> Yojson.Safe.t

JSON adapters
-------------

JSON adapters are a mechanism for rearranging JSON data on-the-fly so as to
make it compatible with ATD, without modifying the ATD type definitions. They
are particularly useful when a JSON API uses conventions that differ from the
ATD defaults — for instance when sum types are encoded as objects with a
``"type"`` discriminator field rather than as ``["Constructor", ...]`` arrays.

The user-provided adapter module must implement two functions:

.. code:: ocaml

    val normalize : Yojson.Safe.t -> Yojson.Safe.t
    (** Convert from the original JSON to ATD-compatible JSON.
        Called before deserialization. *)

    val restore : Yojson.Safe.t -> Yojson.Safe.t
    (** Convert from ATD-compatible JSON back to the original JSON.
        Called after serialization. *)

Field ``adapter.ocaml``
^^^^^^^^^^^^^^^^^^^^^^^

Position: on a sum type or record type expression

Value: an OCaml module identifier providing ``normalize`` and ``restore``

Example — adapting a type-field encoded sum type:

.. code:: ocaml

    type image = { url: string }
    type text  = { title: string; body: string }

    type document = [
      | Image of image
      | Text of text
    ] <json adapter.ocaml="My_adapter">

With the following adapter module:

.. code:: ocaml

    (* My_adapter.ml *)
    (* Converts {"type": "Image", ...rest} <-> ["Image", {...rest}] *)
    let normalize = function
      | `Assoc fields ->
          let tag = match List.assoc_opt "type" fields with
            | Some (`String s) -> s
            | _ -> failwith "missing 'type' field"
          in
          let rest = List.filter (fun (k, _) -> k <> "type") fields in
          `List [`String tag; `Assoc rest]
      | x -> x

    let restore = function
      | `List [`String tag; `Assoc rest] ->
          `Assoc (("type", `String tag) :: rest)
      | x -> x

ATD-compatible JSON values for ``document``:

- ``["Image", {"url": "https://example.com/pic.jpg"}]``
- ``["Text", {"title": "Hello", "body": "World"}]``

Original JSON values (as produced and consumed by the adapter):

- ``{"type": "Image", "url": "https://example.com/pic.jpg"}``
- ``{"type": "Text", "title": "Hello", "body": "World"}``

Fields ``adapter.to_ocaml`` and ``adapter.from_ocaml``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An alternative form that allows specifying the ``normalize`` and ``restore``
functions as inline OCaml expressions, without requiring a dedicated module:

.. code:: ocaml

    type document = [
      | Image of image
      | Text of text
    ]
    <json
      adapter.to_ocaml="My_adapter.normalize"
      adapter.from_ocaml="My_adapter.restore"
    >

Both fields must be provided together. The values are OCaml expressions of
type ``Yojson.Safe.t -> Yojson.Safe.t``.

Import declarations
-------------------

Atdml supports ATD ``import`` declarations that reference types defined in
other ATD modules.

Syntax
^^^^^^

.. code:: ocaml

    import module_name
    import long.module.path as alias

The imported module maps to an OCaml module. For a simple import like
``import base_types``, atdml expects an OCaml module ``Base_types`` (the
last component of the path, capitalised) to be available in scope.

For a dotted path like ``import long.module.path``, atdml expects the
OCaml module path ``Long.Module.Path`` to be in scope.

For an alias like ``import long.module.path as ext``, the generated code
uses ``Ext`` as the local module name and wraps it with
``module Ext = Long.Module.Path`` in the generated ``.ml`` file. The
generated ``.mli`` uses the full ``Long.Module.Path`` name directly.

Language-specific name annotation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``<ocaml name="M">`` annotation can override the OCaml module name
used for the import, on either the path or the alias:

.. code:: ocaml

    import foo <ocaml name="Foo_module">
    import long.path as ext <ocaml name="External">

Example
^^^^^^^

Given ``base_types.atd``:

.. code:: ocaml

    type person_name = string

And ``greeting.atd``:

.. code:: ocaml

    import base_types

    type greeting = {
      name: base_types.person_name;
      message: string;
    }

Atdml generates ``greeting.mli``:

.. code:: ocaml

    type greeting = {
      name: Base_types.person_name;
      message: string;
    }

    val make_greeting : name:Base_types.person_name -> message:string -> unit -> greeting
    val greeting_of_yojson : Yojson.Safe.t -> greeting
    val yojson_of_greeting : greeting -> Yojson.Safe.t
    ...
