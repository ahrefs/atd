(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type ov_mapping =
    (Ocaml.Repr.t, Validate.validate_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> ('a * (Ocaml.Repr.t, Validate.validate_repr) Mapping.def list) list
