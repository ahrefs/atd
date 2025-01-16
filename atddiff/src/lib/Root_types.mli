(*
   Take a list of allowed root types and the contents of an ATD file
   and return a list of types that can't be reached from the allowed
   root types.

   A root type is a type name that is not reachable from the definition
   of another root type.

   Note that the assignment of root types is not unique due to
   mutually-recursive definitions. The result is a list of type names
   that, if added to the list of allowed root types, allows reaching
   all the other type names. We try to keep this list small but we don't
   guarantee that it's the smallest possible.
*)
val check_root_types_superset :
  string list -> Atd.Ast.full_module -> string list
