(*
   Convert (tediously) the atddiff findings to JSON.
*)

val to_string :
  with_locations:bool ->
  Atddiff_output_t.result -> string
