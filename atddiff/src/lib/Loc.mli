(*
   Yet another Loc module.

   This one differs from Atd.Loc because it uses a JSON-friendly type.
*)

type t = Atddiff_output_t.location

val of_atd_loc : Atd.Loc.t -> t

val compare : t -> t -> int

(* Produce something like

     File "atddiff/src/bin/Atddiff_main.ml", lines 247-256, characters 8-5
*)
val to_string : t -> string
