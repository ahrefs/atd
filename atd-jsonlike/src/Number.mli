(** The possible representations of a JSON number.

    For the JSON literal [42], the fields should be all filled as follows:
{v
      { int = Some 42; float = Some 42.; literal = Some "42" }
v}
    JSON numbers that can be represented as an OCaml float (IEEE 754
    binary64) but not as an int will look like this:
{v
      { int = None; float = Some 1.2; literal = Some "1.2" }
v}
    In rare cases, a number literal may be valid syntactically but can't
    be represented as an int or a float. In this case, the JSON literal
    may still be used to convert the number to an int64 or to something
    else. Here's an example:
{v
      { int = None; float = None; literal = Some "1e400" }
v}

    Rules:
    - The literal must always be a valid JSON number literal. For example,
      the YAML literal [+42] is not a valid JSON literal.
    - At least one of the fields [int], [float], or [literal] must be
      set (to [Some ...]).
    - If multiple fields are set, they must be set consistently.
*)

type t = private {
  int: int option;
  float: float option;
  literal: string option;
}

val of_int : int -> t
val of_float : float -> t
val of_string_opt : string -> t option
