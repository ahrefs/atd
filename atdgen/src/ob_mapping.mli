(** OCaml-Biniou decorated ATD AST. *)

type ob_mapping =
    (Ocaml_repr.t, Biniou.biniou_repr) Mapping.t

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> (bool * (Ocaml_repr.t, Biniou.biniou_repr) Mapping.def list) list
