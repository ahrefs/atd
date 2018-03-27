(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type ov_mapping =
    (Ocaml.Repr.t, Validate.validate_repr) Mapping.mapping

val defs_of_atd_modules :
  ('a *
   [< `Type of
        Atd.Ast.loc * (string * string list * Atd.Annot.t) * Atd.Ast.type_expr &
        'b * (string * 'c * 'd) * Atd.Ast.type_expr ]
     list)
    list ->
  ('a *
   (Ocaml.Repr.t, Validate.validate_repr) Mapping.def list)
    list
