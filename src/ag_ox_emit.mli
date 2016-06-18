type 'a expr = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.mapping
type 'a def = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.def
type 'a grouped_defs = (bool * 'a def list) list

val get_full_type_name : (_, _) Ag_mapping.def -> string

val is_exportable : (_, _) Ag_mapping.def -> bool

val make_record_creator
  : ((Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.mapping
     -> (Ag_ocaml.atd_ocaml_repr, 'b) Ag_mapping.mapping)
  -> (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.def
  -> string * string

val opt_annot : string option -> string -> string

val opt_annot_def : string option -> string -> string

val insert_annot : string option -> string

val get_type_constraint
  : original_types:(string, string * int) Hashtbl.t
  -> ('a, 'b) Ag_mapping.def
  -> string

val is_function : Ag_indent.t list -> bool

val needs_type_annot : _ expr -> bool

val check : _ grouped_defs -> unit

val write_ocaml : [< `Files of string | `Stdout ] -> string -> string -> unit
