(*
   Read ATD annotations relating to OCaml.
*)

val annot_schema_of_target : Ocaml_repr.target -> Atd.Annot.schema

val get_ocaml_sum :
  Ocaml_repr.target -> Atd.Annot.t -> Ocaml_repr.atd_ocaml_sum
val get_ocaml_record :
  Ocaml_repr.target -> Atd.Annot.t -> Ocaml_repr.atd_ocaml_record
val get_ocaml_field_prefix : Ocaml_repr.target -> Atd.Annot.t -> string
val get_ocaml_list :
  Ocaml_repr.target -> Atd.Annot.t -> Ocaml_repr.atd_ocaml_list
val get_ocaml_wrap :
  type_param:string list -> Ocaml_repr.target -> Atd.Ast.loc ->
  Atd.Annot.t -> Ocaml_repr.atd_ocaml_wrap option
val get_ocaml_int :
  Ocaml_repr.target -> Atd.Annot.t -> Ocaml_repr.atd_ocaml_int
val get_ocaml_cons : Ocaml_repr.target -> string -> Atd.Annot.t -> string
val get_ocaml_fname : Ocaml_repr.target -> string -> Atd.Annot.t -> string
val get_ocaml_mutable : Ocaml_repr.target -> Atd.Annot.t -> bool
val get_ocaml_predef : Ocaml_repr.target -> Atd.Annot.t -> bool
val get_ocaml_default : Ocaml_repr.target -> Atd.Annot.t -> string option

val get_ocaml_type_name :
  Ocaml_repr.env -> Atd.Ast.loc -> Atd.Ast.type_name ->
  Atd.Annot.t -> Ocaml_repr.t

val get_ocaml_module_and_t :
  Ocaml_repr.target ->
  string ->
  Atd.Annot.t ->
  (string * string * string) option

val get_type_attrs : Atd.Annot.t -> string list
