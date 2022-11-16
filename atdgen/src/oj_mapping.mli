(** OCaml-Json decorated ATD AST. *)

type t = (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.mapping
type variant_mapping =
  (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.variant_mapping

val defs_of_def_groups
  : (bool * Atd.Ast.type_def list) list
  -> target:Ocaml.target
  -> (bool * (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.def list) list
