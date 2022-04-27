(**
   Translate an ATD file to JSON Schema, honoring the <json ...> annotations.
*)

(** Translate an ATD AST to a JSON Schema. *)
val print :
  ?root_id_uri:string ->
  src_name:string ->
  out_channel -> Ast.full_module -> unit
