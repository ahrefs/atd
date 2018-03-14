type 'a expr = (Ocaml.atd_ocaml_repr, 'a) Mapping.mapping
type 'a def = (Ocaml.atd_ocaml_repr, 'a) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

val get_full_type_name : (_, _) Mapping.def -> string

val is_exportable : (_, _) Mapping.def -> bool

val make_record_creator
  : ((Ocaml.atd_ocaml_repr, 'a) Mapping.mapping
     -> (Ocaml.atd_ocaml_repr, 'b) Mapping.mapping)
  -> (Ocaml.atd_ocaml_repr, 'a) Mapping.def
  -> string * string

val opt_annot : string option -> string -> string

val opt_annot_def : string option -> string -> string

val insert_annot : string option -> string

val get_type_constraint
  : original_types:(string, string * int) Hashtbl.t
  -> ('a, 'b) Mapping.def
  -> string

val is_function : Indent.t list -> bool

val needs_type_annot : _ expr -> bool

val check : _ grouped_defs -> unit

val write_ocaml : [< `Files of string | `Stdout ] -> string -> string -> unit

val name_of_var : string -> string

val nth : Mapping.loc_id -> int -> int -> Mapping.loc_id

val map : (bool -> 'a -> 'b) -> 'a list -> 'b list

val get_let : is_rec:bool -> is_first:bool -> Mapping.loc_id * Mapping.loc_id
