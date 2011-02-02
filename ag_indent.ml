(* $Id: ag_indent.ml 45633 2010-07-26 18:26:02Z martin $ *)

(*
  Atd_indent extended with annnotations allowing some postprocessing.
*)

type t = 
    [
    | `Line of string   (* single line (not indented) *)
    | `Block of t list  (* indented sequence *)
    | `Inline of t list (* in-line sequence (not indented) *)
    | `Annot of (string * t)
    ]

let rec strip : t -> Atd_indent.t = function
    `Line _ as x -> x
  | `Block l -> `Block (List.map strip l)
  | `Inline l -> `Inline (List.map strip l)
  | `Annot (_, x) -> strip x
