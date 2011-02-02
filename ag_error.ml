(* $Id: ag_error.ml 45868 2010-07-29 21:45:28Z martin $ *)

open Printf

let error loc msg =
  failwith (sprintf "%s:\n%s" (Atd_ast.string_of_loc loc) msg)

let error2 loc1 msg1 loc2 msg2 =
  failwith (sprintf "%s:\n%s\n%s:\n%s"
              (Atd_ast.string_of_loc loc1) msg1
              (Atd_ast.string_of_loc loc2) msg2)
