(*
   Convert (tediously) the atddiff findings to JSON.
*)

(*
open Atddiff_output_t
*)
let to_yojson x : Yojson.Safe.t =
  `List []

let to_string x =
  x
  |> to_yojson
  |> Yojson.Safe.to_string
  |> Yojson.Safe.prettify
