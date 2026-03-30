(** Position (index) within a source file *)

(** Position within a file using zero-based matrix-like notation.

    Due to differences in line endings between Windows and Unix systems
    and conversions occurring when reading files, we don't track the
    byte offset from the beginning of the file.
*)
type t = {
  row: int;
    (** line number, starting from 0 *)

  column: int;
    (** position within the line, starting from 0 *)
}

(* TODO: equal, compare *)
