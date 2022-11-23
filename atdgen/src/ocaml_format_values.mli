(*
   Helper functions for translating ATD types into OCaml expressions.
   For conversions to OCaml types, see Ocaml_format_types.
*)

val get_implicit_ocaml_default : (Ocaml_repr.t, 'b) Mapping.t -> string option
