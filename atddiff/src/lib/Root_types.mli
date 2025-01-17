(*
   Take a list of possible root types and the contents of an ATD file
   and identify if some type definitions remain unvisited after visiting
   all the definitions the root types depend on.

   Return (unvisited, unvisited_and_unreferenced) where unvisited is the full
   list of type definitions that were not visited and
   unvisited_and_unreferenced is the subset of these unvisited definitions
   that are known to be root types. Other root types may exist in recursive
   definitions but we don't report them.
*)
val check_root_types_superset :
  root_types_superset:string list ->
  ast_with_inherits:Atd.Ast.full_module ->
  string list * string list
