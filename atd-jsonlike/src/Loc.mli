(** Location within a source file, primarily used in error reporting *)

(** A location is a range of positions within an input file or input string.

    [start] and [end_] are positions such that for an empty region at position
    [start], [start = end_].
*)
type t = {
  start: Pos.t;
  end_: Pos.t;

  path: string option;
    (** If provided, [path] must hold a valid path within the local file
        system. *)
}

(* TODO: equal, compare *)
