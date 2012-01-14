
(*
  Optimization of the biniou representation
*)

open Ag_mapping
open Ag_ob_mapping

let get_table_info deref x =
  match deref x with
      `Record y -> y
    | _ ->
	Ag_error.error (Atd_ast.loc_of_type_expr x) "Not a record type"
