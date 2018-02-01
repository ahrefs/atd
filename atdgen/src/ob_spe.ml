
(*
  Optimization of the biniou representation
*)

let get_table_info deref x =
  match deref x with
      `Record y -> y
    | _ ->
        Ag_error.error (Atd_ast.loc_of_type_expr x) "Not a record type"
