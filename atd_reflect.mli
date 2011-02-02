(* $Id: atd_reflect.mli 46594 2010-08-11 09:04:42Z martin $ *)

(**
  Conversion of an AST value into OCaml source code that creates this value
*)

val print_full_module_def : Buffer.t -> string -> Atd_ast.full_module -> unit
  (**
     [print_full_module_def buf name x] prints OCaml source code
     that would construct the given ATD tree [x] and call it [name].
  *)
