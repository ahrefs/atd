(** Utilities for interpreting annotations of type {!Ast.annot} *)

type t = Ast.annot
    (**
       Sample annotation in ATD syntax with legend:
{v

section a   section b
----------- ------
<a a1="x1"> <b b1>
 | |  |        |
 | |  |        +- field without a value
 | |  |
 | |  +- field value
 | |
 | +- field name
 |
 +- section name
v}

       The following rules must be followed for a proper use of this module:

       - There may not be two sections with the same name. The [merge] function
       can be used to merge sections.
       - Section order doesn't matter as long as section names are unique.
       - Field names within a section should be unique. If not, only the
       first occurrence of each field is meaningful.
       - Field values may be arbitrary strings.
    *)

val has_section : string -> t -> bool
  (** Return true if such a section (first-level key) exists. *)

val has_field : sections:string list -> field:string -> t -> bool
  (** [has_field section_names field_name annotations]
      returns true if at least one section exists with
      a name in [section_names] and one of its fields named
      [field_name].

      Each section should be unique.
  *)

val get_flag : sections:string list -> field:string -> t -> bool
  (** [get_flag section_names field_name]
      looks sequentially into the sections specified by [section_names]
      for a field named [field_name].
      If no such field can be found in any section, the result is [false].
      Otherwise, the search stops with the first field matching
      one of the section names and the field name.
      If the field has no associated value, the result is [true].
      If the field value is ["true"] then [true] is returned.
      Likewise, if the field value is ["false"] then [false] is returned.
      If the field value is anything else, a [Failure] exception is raised.

      Each section should be unique.

      Given the following annotations in the ATD syntax noted [a]:
{v
<ocaml openin="false"> <ocaml_312 openin>
v}

      We obtain the following results:

{v
# get_flag \["ocaml_312"; "ocaml"\] "openin" a;;
\- : true
# get_flag \["ocaml_311"; "ocaml"\] "openin" a;;
\- : false
# get_flag \["ocaml_312"\] "openin" a;;
\- : true
# get_flag \["ocaml_311"\] "openin" a;;
\- : false
# get_flag \["ocaml"\] "openin" a;;
\- : false
v}
  *)

val get_field :
  parse:(string -> 'a option) ->
  default:'a ->
  sections:string list ->
  field:string ->
  t -> 'a
  (** [get_field parse default section_names field_name annotations]
      looks sequentially into the sections specified by [section_names]
      for a field named [field_name].
      If no such field exists, the [default] value is returned.
      If the field is present, the associated value is parsed using
      the given function [parse] which should return [None]
      in order to indicate an invalid value.
      If the field is present without an associated value
      or if [parse] returns [None], a [Failure] exception is raised.

      Each section should be unique.
  *)

val get_fields :
  parse:(string -> 'a option) ->
  sections:string list ->
  field:string ->
  t -> 'a list
  (** [get_fields parse section_names field_name annotations]
      looks sequentially into the sections specified by [section_names]
      for fields named [field_name].
      Each found value is parsed using the given function [parse] which
      should return [None] in order to indicate an invalid value.
      In the end the list of parsed values is returned.
      If the field is present without an associated value
      or if [parse] returns [None], a [Failure] exception is raised.

      Each section should be unique.
  *)

val get_opt_field :
  parse:(string -> 'a option) ->
  sections:string list ->
  field:string ->
  t -> 'a option
  (** This is the same as [get_field] except that no default value
      for the field exists. *)

val get_loc :
  sections:string list ->
  field:string ->
  t -> Ast.loc option
  (** Returns the location of the annotation. Useful for error
      reporting. *)

val get_loc_exn :
  sections:string list ->
  field:string ->
  t -> Ast.loc
  (** Returns the location of the annotation. Useful for error
      reporting. *)


val set_field :
  loc:Ast.loc ->
  section:string ->
  field:string ->
  string option -> t -> t
  (** [set_field loc section_name field_name value annotations]
      sets a field, reusing existing section [section_name]
      if it exists, preserving the position of field [field_name]
      and overwriting its value if it exists.
  *)

val merge : t -> t
  (** Merge sections of the same name together,
      and keeps only the first occurrence of each
      field.

{v
<a a1="x1"> <b> <a a2="x3" a1="x2">
v}

becomes

{v
<a a1="x1" a2="x3"> <b>
v}
  *)

val create_id : unit -> string
  (** Create a unique numeric ID *)

type node_kind =
  | Module_head
  | Type_def
  | Type_expr
  | Variant
  | Cell
  | Field

type schema_field = node_kind * string

(** An annotation schema for a section ["foo"] defines all the locations where
    annotations of the form [<foo ...>] can occur and which fields are
    allowed.
    The goal is to detect misspellings in field names or the incorrect
    placement of an annotation.

    Annotations whose section is undeclared in the schema are ignored.
    The following is an example specifying the legal placement of annotations
    of the form [<json name="...">]:
{v
    {
       section = "json";
       fields = [
         Variant, "name";
         Field, "name";
       ]
    }
v}
*)
type schema_section = {
  section: string;
  fields: schema_field list;
}

type schema = schema_section list

(** Check that annotations of interest are not misplaced.
    Raises an exception with an error message when the check fails. *)
val validate : schema -> Ast.any -> unit

(**
   Remove annotations which do not have at least one of the provided [tags] found in their tag field.
*)
val filter_by_tags :
  tags : string list ->
  Ast.full_module -> Ast.full_module
