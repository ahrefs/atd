(** OCaml-Biniou decorated ATD AST. *)

type ob_mapping =
    (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.mapping

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> (bool * (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.def list) list
