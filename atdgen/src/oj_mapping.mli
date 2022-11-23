(** OCaml-Json decorated ATD AST. *)

type t = (Ocaml_repr.t, Atd.Json.json_repr) Mapping.t
type variant_mapping =
  (Ocaml_repr.t, Atd.Json.json_repr) Mapping.variant_mapping

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> Ocaml_repr.env
  -> (bool * (Ocaml_repr.t, Atd.Json.json_repr) Mapping.def list) list
