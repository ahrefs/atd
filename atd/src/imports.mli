(**
   Manage external definitions via 'import' statements.

   Here's an import statement:

{v
     import fiz.buz.std_foo as foo
            ^^^^^^^^^^^^^^^
            module name
                               ^^^
                               local alias
v}

   This declares the existence of an external ATD module known globally as
   [fiz.buz.std_foo]. Its local name would normally be the last component
   of the dotted module name ([std_foo]) but since an alias is specified
   with [as], that alias is the local name of the module. Aliases are useful
   for disambiguating module names whose last component are identical
   as well as for shortening them for convenience.

   A dotted type name such as [foo.bar] could occur anywhere as a type
   expression and the presence of two components separated by a dot indicate
   a type that's defined externally. Here's the anatomy of an external type:

{v
     type t = foo.bar list
              ^^^
              local
              module
              name
                  ^^^
                  type name
v}
*)

(** A table holding the imports. *)
type t

(** Load a list of imports and check that their local names are different. *)
val load : Ast.import list -> t

(** Look up a type name to determine if it's defined externally.
    If so, return the information about the imported module
    (e.g. [import fiz.std_foo as foo]). Also return the type's base name
    (last component e.g. [bar] in [foo.bar]).
*)
val resolve : t -> Ast.loc -> Ast.type_name -> (Ast.import option * string)
