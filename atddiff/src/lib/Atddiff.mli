(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type output_format = Text | JSON

(*
  Compare two ATD files and return the result in the requested output format.
*)
val compare_files :
  ?output_format:output_format ->
  string -> string -> string

(* Version of the atddiff library and executable *)
val version : string
