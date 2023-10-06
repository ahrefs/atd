(*
   A location is a region in a source file.
*)

type t = Lexing.position * Lexing.position

(* Compare two locations so as to sort them by:
   1. file path
   2. start position in the file
   3. end position in the file
*)
val compare : t -> t -> int
