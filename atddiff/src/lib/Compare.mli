(*
   Compare two ATD ASTs for incompatibilities

   The findings are sorted nicely.
*)

type sort_by = Location | Hash

type options = {
  (* Are fields with defaults always populated in old JSON data? *)
  json_defaults_old : bool;
  (* Are fields with defaults always populated in new JSON data? *)
  json_defaults_new : bool;
  sort_by: sort_by;
}

val asts :
  options -> Atd.Ast.full_module -> Atd.Ast.full_module ->
  Atddiff_output_t.result
