(** Monomorphization of type definitions *)

val expand_type_defs :
  ?prefix:string ->
  ?keep_builtins:bool ->
  ?keep_poly:bool ->
  ?debug:bool ->
  Ast.type_def list -> Ast.type_def list
(**
   Monomorphization of type expressions.

   @param prefix prefix to use for new type names. Default is ["_"].

   @param keep_builtins preserve occurrences of the built-in parametrized
   types such as [list] or [option].
   Default is [false].

   @param keep_poly return definitions for the parametrized types.
   Default is [false].

   @param debug keep meaningful but non ATD-compliant names for new type names.
   Default is [false].

   The goal is to inline each parametrized type definition as much as possible,
   allowing code generators to create more efficient code directly:

{v
  type ('a, 'b) t = [ Foo of 'a | Bar of 'b ]
  type int_t = (int, int) t
v}

  becomes:

{v
  type int_t = _1
  type _1 = [ Foo of int | Bar of int ]
v}

  A secondary goal is to factor out type subexpressions in order for
  the code generators to produce less code:

{v
  type x = \{ x : int list }
  type y = \{ y : int list option }
v}

  becomes:

{v
  type x = \{ x : _1 }
  type y = \{ y : _2 }
  type _1 = int list   (* `int list' now occurs only once *)
  type _2 = _1 option
v}

  By default, only parameterless type definitions are returned.
  The [keep_poly] option allows to return parametrized type definitions as
  well.

  Input:

{v
  type 'a abs = abstract
  type int_abs = int abs
  type 'a tree = [ Leaf of 'a | Node of ('a tree * 'a tree) ]
  type t = int tree
  type x = [ Foo | Bar ] tree
v}

  Output (pseudo-syntax where quoted strings indicate unique type identifiers):

{v
  type "int abs" = int abs
  type int_abs = "int abs"

  type 'a tree = [ Leaf of 'a | Node of ('a tree * 'a tree) ]
    (* only if keep_poly = true *)

  type "int tree" = [ Leaf of int | Node of ("int tree" * "int tree") ]
  type t = "int tree"
  type "[ Foo | Bar ] tree" =
    [ Leaf of [ Foo | Bar ]
    | Node of ("[ Foo | Bar ] tree" * "[ Foo | Bar ] tree") ]
  type x = "[ Foo | Bar ] tree"
v}

*)
