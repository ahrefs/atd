(** The collection of core types known by ATD. *)

(** List of all predefined type names with their arity and their definition
    if applicable.

    The parametrized types [list], [option], etc. all have a definition
    while the simple types like [bool] and [int] have an entry in the list
    but no associated definition ([None]).
*)
val list : (Ast.type_name * int * Ast.type_def option) list

type table = (Ast.type_name, int * Ast.type_def option) Hashtbl.t

(** Create a lookup table from the list of predefined type definitions
    and extra definitions. *)
val make_table : Ast.type_def list -> table

(** De-alias a type name recursively.
    This is useful to determine the actual type constructor behind a name.
    For this, see also [get_construct].
    For example, looking up ["special"] given the
    ATD definition [type special = int list] would return the definition
    for ["list"].
*)
val get_original_definition :
  table -> Ast.type_name -> (int * Ast.type_def option) option

(** Determine the type construct by returning the right-handside of the
    original type definition associated with type alias.
    For example, looking up ["special"] given the
    ATD definition [type special = int list] would return
    [(1, List ...)] where [1] indicates that the type expression depends on
    one type parameter.
*)
val get_construct :
  table -> Ast.type_name -> (int * Ast.type_expr) option

(** Determine the type construct associated with a type expression.
    The result may be [Name] only if it's a predefined type. If a type
    name is undefined, the result is [None].
    This is intended for determining default values.
*)
val get_construct_of_expr :
  table -> Ast.type_expr -> Ast.type_expr option
