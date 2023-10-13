(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type output_format = Text | JSON

(*
   Compare two ATD files and return the result in the requested output format.

   json_defaults_old: indicates whether fields with defaults are always
                      populated in old JSON data.
   json_defaults_new: indicates whether fields with defaults are always
                      populated in new JSON data.
*)
val compare_files :
  ?json_defaults_old:bool ->
  ?json_defaults_new:bool ->
  ?output_format:output_format ->
  string -> string -> (unit, string) Result.t

(* Version of the atddiff library and executable *)
val version : string
