(**
   Utilities for manipulating type names as found in the ATD AST.

   This module can serve as the input to Map or Set functors. e.g.
{v
     module Names = Set.Make (Type_name)
v}
*)

(** The type of a type name.

    The simple ATD type name [a] is represented as [TN ["a"]].
    The composite ATD type name [a.b.c] is represented as [TN ["a"; "b"; "c"]].

    ATD type definitions normally have a single component.
    The list of path components may not be empty.
    Two components indicate a type provided by an external module.

    At the moment, the semantics of imports doesn't allow type names of
    more than two components.
*)
type t = TN of string list

val compare : t -> t -> int
val equal : t -> t -> bool

(** Format to a string in ATD syntax.
    For example, [TN ["a"; "b"]] gives [a.b]. *)
val to_string : t -> string

(** Return the base name, i.e. the last component in the path. *)
val basename : t -> string

(** Return the module name if any, and the base name.
    An exception is raised if the number of path components is other than
    1 or 2.
*)
val split : t -> string option * string
