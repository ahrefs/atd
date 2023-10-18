(*
   Yet another Loc module.

   This one differs from Atd.Loc because it uses a JSON-friendly type.
*)

module T = Atddiff_output_t

type t = T.location

let position_of_lexing_pos (x : Lexing.position) : T.position =
  {
    path = x.pos_fname;
    line = x.pos_lnum;
    column = x.pos_cnum - x.pos_bol;
  }

let of_atd_loc (start, end_) : t =
  {
    start = position_of_lexing_pos start;
    end_ = position_of_lexing_pos end_
  }

let compare_pos (a : T.position) (b : T.position) =
  let c = String.compare a.path b.path in
  if c <> 0 then c
  else
    let c = Int.compare a.line b.line in
    if c <> 0 then c
    else
      Int.compare a.column b.column

(* Compare two locations so as to sort them by:
   1. file path
   2. start position in the file
   3. end position in the file
*)
let compare (a : t) (b : t) =
  let c = compare_pos a.start b.start in
  if c <> 0 then c
  else
    compare_pos a.end_ b.end_

let to_string ({start; end_} : t) =
  Printf.sprintf "File %S, line %i, characters %i-%i"
    start.path
    start.line
    start.column
    end_.column
