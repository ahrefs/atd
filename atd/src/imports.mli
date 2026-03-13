(**
   Manage external definitions via 'from ... import' statements.

   Here's an import statement:

{v
     from fiz.buz.std_foo as foo import bar, baz
          ^^^^^^^^^^^^^^^
          module name
                           ^^^
                           local alias
                                        ^^^  ^^^
                                        imported types
v}

   This declares that types [bar] and [baz] are defined externally in module
   [fiz.buz.std_foo]. Its local name would normally be the last component
   of the dotted module name ([std_foo]) but since an alias is specified
   with [as], that alias is the local name. Aliases are useful for
   disambiguating module names whose last component are identical as well as
   for shortening them for convenience.

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

(** A table holding the imports, keyed by local module name. *)
type t = (string, Ast.import) Hashtbl.t

(** Load a list of imports, check for duplicate module names and duplicate
    type names within each import, and return a lookup table. *)
val load : Ast.import list -> t

(** Look up a type name to determine if it's defined externally.
    For a qualified name [module.type], returns
    [(Some (import, Some imported_type), base_name)] if [module] is in the
    import table and [type] is in the type list,
    [(Some (import, None), base_name)] if [module] is in the import table but
    [type] is not listed (likely an error caught by [check_type_refs]),
    or [(None, base_name)] for an unqualified name.
*)
val resolve :
  t -> Ast.loc -> Ast.type_name ->
  (Ast.import * Ast.imported_type option) option * string

(** Walk all type expressions in [type_defs] and verify that every qualified
    type reference [a.b] was explicitly listed in the import statement for
    module [a], and that the arity matches the declaration. Raises
    [Ast.Atd_error] on the first violation. *)
val check_type_refs : t -> Ast.type_def list -> unit
