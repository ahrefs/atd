(** Decorated ATD AST for OCaml validators. *)

type ov_mapping =
    (Ocaml_repr.t, Validate.validate_repr) Mapping.t

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> (bool * (Ocaml_repr.t, Validate.validate_repr) Mapping.def list) list
