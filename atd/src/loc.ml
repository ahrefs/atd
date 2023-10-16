(*
   A location is a region in a source file.
*)

type t = Lexing.position * Lexing.position

let compare_pos (a : Lexing.position) (b : Lexing.position) =
  let c = String.compare a.pos_fname b.pos_fname in
  if c <> 0 then c
  else
    Int.compare a.pos_cnum b.pos_cnum

(* Compare two locations so as to sort them by:
   1. file path
   2. start position in the file
   3. end position in the file
*)
let compare ((a_start, a_end) : t) ((b_start, b_end) : t) =
  let c = compare_pos a_start b_start in
  if c <> 0 then c
  else
    compare_pos a_end b_end
