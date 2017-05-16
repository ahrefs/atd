
type atd_ocaml_sum = [ `Classic | `Poly ]
type atd_ocaml_record = [ `Record | `Object ]
type atd_ocaml_int = [ `Int | `Char | `Int32 | `Int64 | `Float ]
type atd_ocaml_list = [ `List | `Array ]
type target = [ `Default | `Biniou | `Json | `Validate ]

type atd_ocaml_wrap = {
  ocaml_wrap_t : string;
  ocaml_wrap : string;
  ocaml_unwrap : string;
}

type atd_ocaml_field = {
  ocaml_default : string option;
  ocaml_fname : string;
  ocaml_mutable : bool;
  ocaml_fdoc : Ag_doc.doc option;
}

type atd_ocaml_variant = {
  ocaml_cons : string;
  ocaml_vdoc : Ag_doc.doc option;
}

type atd_ocaml_def = {
  ocaml_predef : bool;
  ocaml_ddoc : Ag_doc.doc option;
}

type atd_ocaml_repr =
  [ `Unit
  | `Bool
  | `Int of atd_ocaml_int
  | `Float
  | `String
  | `Sum of atd_ocaml_sum
  | `Record of atd_ocaml_record
  | `Tuple
  | `List of atd_ocaml_list
  | `Option
  | `Nullable
  | `Wrap of atd_ocaml_wrap option
  | `Name of string
  | `External of (string * string * string)
        (*
          (module providing the type,
           module providing everything else,
           type name)
        *)

  | `Cell of atd_ocaml_field
  | `Field of atd_ocaml_field
  | `Variant of atd_ocaml_variant
  | `Def of atd_ocaml_def ]

val get_ocaml_sum : Atd_annot.t -> atd_ocaml_sum

val get_ocaml_record : Atd_annot.t -> atd_ocaml_record

val get_ocaml_field_prefix : Atd_annot.t -> string

val get_ocaml_list : Atd_annot.t -> atd_ocaml_list

val get_ocaml_wrap : Atd_ast.loc -> Atd_annot.t -> atd_ocaml_wrap option

val get_ocaml_int : Atd_annot.t -> atd_ocaml_int

val get_ocaml_default : Atd_annot.t -> string option

val get_ocaml_cons : string -> Atd_annot.t -> string

val get_ocaml_fname : string -> Atd_annot.t -> string

val get_ocaml_mutable : Atd_annot.t -> bool

val get_ocaml_predef : target -> Atd_annot.t -> bool

val get_ocaml_module_and_t
  : target
  -> string
  -> Atd_annot.t
  -> (string * string * string) option


val get_implicit_ocaml_default
  : ('a ->
     [> `Bool of 'b * [> `Bool ] * 'c
     | `Float of 'd * [> `Float ] * 'e
     | `Int of
          'f * [> `Int of [< `Char | `Float | `Int | `Int32 | `Int64 ] ] * 'g
     | `List of 'h * 'i * [> `List of [> `Array | `List ] ] * 'j
     | `Nullable of 'k * 'l * [> `Nullable ] * 'm
     | `Option of 'n * 'o * [> `Option ] * 'p
     | `String of 'q * [> `String ] * 'r
     | `Unit of 's * [> `Unit ] * 't ])
  -> 'a
  -> string option

val unwrap_option
  : ('a -> ('b, 'c) Ag_mapping.mapping)
  -> 'a
  -> ('b, 'c) Ag_mapping.mapping

val ocaml_of_atd
  : ?pp_convs:[ `Camlp4 of string list | `Ppx of string list ]
  -> target:target
  -> type_aliases:string option
  -> (Atd_ast.loc * Atd_ast.annot) * (bool * Atd_ast.module_body) list
  -> string


val map_record_creator_field
  : ((atd_ocaml_repr, 'a) Ag_mapping.mapping
     -> (atd_ocaml_repr, 'b) Ag_mapping.mapping)
  -> (atd_ocaml_repr, 'a) Ag_mapping.field_mapping
  -> string * string * string
