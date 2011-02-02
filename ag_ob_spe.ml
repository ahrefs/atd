(* $Id: ag_ob_spe.ml 45868 2010-07-29 21:45:28Z martin $ *)

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
