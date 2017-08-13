
type o = Ag_ocaml.atd_ocaml_repr
type j = Ag_json.json_repr

type oj_mapping =
    (Ag_ocaml.atd_ocaml_repr, Ag_json.json_repr) Ag_mapping.mapping

val defs_of_atd_modules
  : ('a *
     [< `Type of
          Atd_ast.loc * (string * string list * Atd_annot.t) * Atd_ast.type_expr ]
       list
    ) list
  -> ('a * (Ag_ocaml.atd_ocaml_repr, Ag_json.json_repr) Ag_mapping.def list) list
