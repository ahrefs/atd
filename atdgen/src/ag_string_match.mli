
(*
  Compilation of string pattern matching into something
  supposedly faster than what ocamlopt does.
*)

type position = [ `Length | `Position of int | `End ]
type value = [ `Int of int | `Char of char ]

type 'a tree =
    [ `Node of (position * (value * 'a tree) list)
    | `Branch of ((position * value) list * 'a tree)
    | `Leaf of 'a ]

val make_tree : (string * 'a) list -> 'a tree

type exit_with =
    [ `Exn of string
    | `Expr ]
      (** [`Exn s] raises an exception for each failure branch, and this
          exception is caught in one place, avoiding duplication of the
          [error_expr] expression.

          [`Expr] uses the [error_expr] in each failure branch,
          resulting in code duplication but avoiding raising and
          catching an exception. Suitable for fixed-length values
          for which code duplication is tolerable.
      *)

val make_ocaml_expr_factored :
  ?string_id: string ->
  ?pos_id: string ->
  ?len_id: string ->
  ?exit_with: exit_with ->
  error_expr: Ag_indent.t list ->
  (string option * Ag_indent.t list) list -> Ag_indent.t list

val make_ocaml_expr_naive :
  ?string_id: string ->
  ?pos_id: string ->
  ?len_id: string ->
  error_expr: Ag_indent.t list ->
  (string option * Ag_indent.t list) list -> Ag_indent.t list

val make_ocaml_expr :
  optimized: bool ->
  ?string_id: string ->
  ?pos_id: string ->
  ?len_id: string ->
  ?exit_with: exit_with ->
  error_expr: Ag_indent.t list ->
  (string option * Ag_indent.t list) list -> Ag_indent.t list


val make_ocaml_int_mapping :
  ?string_id: string ->
  ?pos_id: string ->
  ?len_id: string ->
  ?exit_with: exit_with ->
  error_expr1: Ag_indent.t list ->
  ?error_expr2: Ag_indent.t list ->
  ?int_id: string ->
  (string option * Ag_indent.t list) list ->

  (Ag_indent.t list * Ag_indent.t list)
    (*
      takes a list of cases, each being defined by a string to match against
      and by a corresponding expression of type 'a.

      returns:
      - function expression of type string -> int -> int -> int
        (maps a substring to an int corresponding to one of the strings
        to match against)

      - match-with expression of type 'a
        (matches s against the ints corresponding to the strings to match
        against)

      The whole point is to read records or variants without
      creating new strings or closures.
    *)
