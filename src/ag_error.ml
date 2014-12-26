
open Printf

let error loc msg =
  failwith (sprintf "%s:\n%s" (Atd_ast.string_of_loc loc) msg)

let error2 loc1 msg1 loc2 msg2 =
  failwith (sprintf "%s:\n%s\n%s:\n%s"
              (Atd_ast.string_of_loc loc1) msg1
              (Atd_ast.string_of_loc loc2) msg2)

let error3 loc1 msg1 loc2 msg2 loc3 msg3 =
  failwith (sprintf "%s:\n%s\n%s:\n%s\n%s:\n%s"
              (Atd_ast.string_of_loc loc1) msg1
              (Atd_ast.string_of_loc loc2) msg2
              (Atd_ast.string_of_loc loc3) msg3)
