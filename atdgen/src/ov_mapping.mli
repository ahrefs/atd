(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type ov_mapping =
    (Ocaml.atd_ocaml_repr, Validate.validate_repr) Mapping.mapping

val defs_of_atd_modules :
  ('a *
   [< `Type of
        Atd_ast.loc * (string * string list * Atd_annot.t) * Atd_ast.type_expr &
        'b * (string * 'c * 'd) * Atd_ast.type_expr ]
     list)
    list ->
  ('a *
   (Ocaml.atd_ocaml_repr, Validate.validate_repr) Mapping.def list)
    list
