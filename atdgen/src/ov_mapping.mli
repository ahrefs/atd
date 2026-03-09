(** Decorated ATD AST for OCaml validators. *)

type ov_mapping =
    (Ocaml.Repr.t, Validate.validate_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.type_def list) list
  -> ('a * (Ocaml.Repr.t, Validate.validate_repr) Mapping.def list) list
