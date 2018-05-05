(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

type t = (Ocaml.Repr.t, Json.json_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> ('a * (Ocaml.Repr.t, Json.json_repr) Mapping.def list) list

(** "A.B" -> "A.B.normalize" *)
val json_normalizer_of_adapter_path : string -> string

(** "A.B" -> "A.B.restore" *)
val json_restorer_of_adapter_path : string -> string
