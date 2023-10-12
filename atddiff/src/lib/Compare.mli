(*
   Compare two ATD ASTs for incompatibilities

   The findings are sorted nicely.
*)

type options = {
  (* Are fields with defaults always populated in old JSON data? *)
  json_defaults_old : bool;
  (* Are fields with defaults always populated in new JSON data? *)
  json_defaults_new : bool;
}

val asts :
  options -> Atd.Ast.full_module -> Atd.Ast.full_module -> Types.result
