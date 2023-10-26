(*
   Format comparison results in a human-readable form
*)

val to_string :
  with_locations:bool ->
  Atddiff_output_t.result -> string
