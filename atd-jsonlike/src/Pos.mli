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

val zero : t
(** Position at the start of an input: row 0, column 0. *)

val equal : t -> t -> bool
val compare : t -> t -> int
