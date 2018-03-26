(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type o = Ocaml.atd_ocaml_repr
type j = Json.json_repr

type oj_mapping =
    (Ocaml.atd_ocaml_repr, Json.json_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a *
     [< `Type of
          Atd.Ast.loc * (string * string list * Atd.Annot.t) * Atd.Ast.type_expr ]
       list
    ) list
  -> ('a * (Ocaml.atd_ocaml_repr, Json.json_repr) Mapping.def list) list

val json_normalizer_of_adapter_path : Json.json_adapter -> string
val json_restorer_of_adapter_path : Json.json_adapter -> string
