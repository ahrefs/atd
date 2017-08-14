(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type ov_mapping =
    (Ag_ocaml.atd_ocaml_repr, Ag_validate.validate_repr) Ag_mapping.mapping

val defs_of_atd_modules :
  ('a *
   [< `Type of
        Atd_ast.loc * (string * string list * Atd_annot.t) * Atd_ast.type_expr &
        'b * (string * 'c * 'd) * Atd_ast.type_expr ]
     list)
    list ->
  ('a *
   (Ag_ocaml.atd_ocaml_repr, Ag_validate.validate_repr) Ag_mapping.def list)
    list
