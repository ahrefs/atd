(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type output_format = Text | JSON

let version = Version.version

let format_json res : string =
  failwith "JSON output: not implemented"

let compare_files
      ?(output_format = Text)
      old_file new_file =
  let load_file atd_file =
    atd_file
    |> Atd.Util.load_file
      ~expand:true (* removes parametrized types, preserves locations
                      where type name substitution occurs *)
      ~keep_poly:true (* keeps polymorphic type defs *)
      ~inherit_fields:true (* simplifies comparison *)
      ~inherit_variants:true (* simplifies comparison *)
    |> fst
    |> Atd.Ast.remove_wrap_constructs
  in
  let ast1 = load_file old_file in
  let ast2 = load_file new_file in
  let res = Compare.asts ast1 ast2 in
  match output_format with
  | Text -> Format_text.to_string res
  | JSON -> format_json res
