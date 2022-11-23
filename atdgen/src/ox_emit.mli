(** Utilities for writing OCaml code generators from a decorated ATD AST. *)

type 'a expr = (Ocaml_repr.t, 'a) Mapping.t
type 'a def = (Ocaml_repr.t, 'a) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type target =
  | Files of string
  | Stdout

val get_full_type_name : (_, _) Mapping.def -> string

val is_exportable : (_, _) Mapping.def -> bool

val make_record_creator
  : ((Ocaml_repr.t, 'a) Mapping.t
     -> (Ocaml_repr.t, 'b) Mapping.t)
  -> (Ocaml_repr.t, 'a) Mapping.def
  -> string * string

val opt_annot : string option -> string -> string

val opt_annot_def : string option -> string -> string

val insert_annot : string option -> string

val get_type_constraint : ('a, 'b) Mapping.def -> string

(** Determine whether the start of the given block of code was annotated
    with the "fun" tag, indicating that it represents a lambda (anonymous
    function). *)
val is_lambda : Indent.t list -> bool

val is_function : Indent.t list -> bool
[@@deprecated "Please use 'is_lambda' instead of 'is_function'."]

val needs_type_annot : _ expr -> bool

val check : _ grouped_defs -> unit

val write_ocaml : target -> string -> string -> unit

val name_of_var : string -> string

val nth : string -> int -> int -> string

val get_let : is_rec:bool -> is_first:bool -> string * string

val write_opens : Buffer.t -> string list -> unit

val def_of_atd
  : Atd.Ast.type_def
  -> env:Ocaml_repr.env
  -> def:'a
  -> external_:'a
  -> mapping_of_expr:(Atd.Ast.type_expr -> (Ocaml_repr.t, 'a) Mapping.t)
  -> (Ocaml_repr.t, 'a) Mapping.def

val maybe_write_creator_impl
  : with_create:bool
  -> ((Ocaml_repr.t, 'a) Mapping.t ->
      (Ocaml_repr.t, 'b) Mapping.t)
  -> Buffer.t
  -> ('c * (Ocaml_repr.t, 'a) Mapping.def list) list
  -> unit

val maybe_write_creator_intf
  : with_create:bool
  -> ((Ocaml_repr.t, 'a) Mapping.t ->
      (Ocaml_repr.t, 'b) Mapping.t)
  -> Buffer.t
  -> (Ocaml_repr.t, 'a) Mapping.def
  -> unit

val default_value
  : (Ocaml_repr.t, 'a) Mapping.field_mapping
  -> ((Ocaml_repr.t, 'a) Mapping.t -> (Ocaml_repr.t, 'b) Mapping.t)
  -> string option

val include_intf : (Ocaml_repr.t, 'a) Mapping.def -> bool

type field =
  { mapping : (Ocaml_repr.t, Atd.Json.json_repr) Mapping.field_mapping
  ; ocaml_fname : string
  ; json_fname : string
  ; ocaml_default : string option
  ; optional : bool
  ; unwrapped : bool
  }

val get_fields
  : ((Ocaml_repr.t, Atd.Json.json_repr) Mapping.t
     -> (Ocaml_repr.t, 'a) Mapping.t)
  -> (Ocaml_repr.t, Atd.Json.json_repr) Mapping.field_mapping array
  -> field list

val is_string :
  (('a, 'b) Mapping.t -> ('a, 'b) Mapping.t)
  -> ('a, 'b) Mapping.t
  -> bool

val get_assoc_type : ((Ocaml_repr.t, Atd.Json.json_repr) Mapping.t ->
  (Ocaml_repr.t, Atd.Json.json_repr) Mapping.t) ->
    Mapping.loc ->
      (Ocaml_repr.t, Atd.Json.json_repr) Mapping.t ->
        (Ocaml_repr.t, Atd.Json.json_repr) Mapping.t *
          (Ocaml_repr.t, Atd.Json.json_repr) Mapping.t
