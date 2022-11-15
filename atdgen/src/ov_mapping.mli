(** Decorated ATD AST for OCaml validators. *)

type ov_mapping =
    (Ocaml.Repr.t, Validate.validate_repr) Mapping.mapping

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> (bool * (Ocaml.Repr.t, Validate.validate_repr) Mapping.def list) list
