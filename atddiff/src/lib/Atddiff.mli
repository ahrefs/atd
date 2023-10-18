(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type simple_filter =
  (* A finding reported to affect the given ATD type name.
     It's possible that this type name exists only in one of the two versions
     of the ATD file. *)
  | Affected_type_name of string
  (* Select backward incompatibilies *)
  | Backward
  (* Select forward incompatibilies *)
  | Forward

(* The type of a filter over the findings.

   Command-line options that describe filters are translated to this type.

   This query language isn't exposed by the CLI right now because it's
   overkill and it would require a special parser.
*)
type filter =
  | Or of filter list (* union; none = Or [] *)
  | And of filter list (* intersection; all = And [] *)
  | Not of filter (* set difference *)
  | Filter of simple_filter

type output_format = Text | JSON

(*
   Compare two ATD files and return the result in the requested output format.

   json_defaults_old: indicates whether fields with defaults are always
                      populated in old JSON data.
   json_defaults_new: indicates whether fields with defaults are always
                      populated in new JSON data.
*)
val compare_files :
  ?filter:filter ->
  ?json_defaults_old:bool ->
  ?json_defaults_new:bool ->
  ?output_format:output_format ->
  string -> string -> (unit, string) Result.t

(* Version of the atddiff library and executable *)
val version : string
