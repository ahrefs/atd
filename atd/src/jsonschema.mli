(**
   Translate an ATD file to JSON Schema, honoring the <json ...> annotations.
*)

(** Translate an ATD AST to a JSON Schema. *)
val print :
  src_name:string ->
  root_type:string ->
  out_channel -> Ast.full_module -> unit
