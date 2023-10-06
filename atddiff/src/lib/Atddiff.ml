(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type output_format = Text | JSON

let compare_files
      ?(output_format = Text)
      old_file new_file =
  "not implemented. so sad."

let version = Version.version
