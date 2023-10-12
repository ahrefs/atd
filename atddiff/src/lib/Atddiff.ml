(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type output_format = Text | JSON

let version = Version.version

let format_json res : string =
  failwith "JSON output: not implemented"

let compare_files
  ?(json_defaults_old = false)
  ?(json_defaults_new = false)
  ?(output_format = Text)
    old_file new_file =
  let load_file atd_file =
    atd_file
    |> Atd.Util.load_file
      ~inherit_fields:true (* simplifies comparison *)
      ~inherit_variants:true (* simplifies comparison *)
    |> fst
    |> Atd.Ast.remove_wrap_constructs
  in
  let ast1 = load_file old_file in
  let ast2 = load_file new_file in
  let res =
    let options : Compare.options = {
      json_defaults_old;
      json_defaults_new
    } in
    Compare.asts options ast1 ast2 in
  match output_format with
  | Text -> Format_text.to_string res
  | JSON -> format_json res
