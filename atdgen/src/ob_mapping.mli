(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type ob_mapping =
    (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.mapping


val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> ('a * (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.def list) list
