(*
   Compare two ATD ASTs for incompatibilities

   The findings are sorted nicely.
*)
val asts : Atd.Ast.full_module -> Atd.Ast.full_module -> Types.result
