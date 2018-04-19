
(*
  Atd.Indent extended with annnotations allowing some postprocessing.
*)

type t =
    [
    | `Line of string        (* single line (not indented) *)
    | `Block of t list       (* indented sequence *)
    | `Inline of t list      (* in-line sequence (not indented) *)
    | `Annot of (string * t) (* arbitrary annotation *)
    ]

let rec strip : t -> Atd.Indent.t = function
    `Line _ as x -> x
  | `Block l -> `Block (List.map strip l)
  | `Inline l -> `Inline (List.map strip l)
  | `Annot (_, x) -> strip x

let rec concat t = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::t::(concat t xs)
