(*
    Internal Atddiff library used by the 'atddiff' command.
*)

type simple_filter =
  | Affected_type_name of string
  | Backward
  | Forward

type filter =
  | Or of filter list
  | And of filter list
  | Not of filter
  | Filter of simple_filter

type output_format = Text | JSON

let version = Version.version

let format_json res : string =
  failwith "JSON output: not implemented"

let rec select_finding filter (x : Types.finding * string list) =
  match filter with
  | Or filters ->
      List.exists (fun filter -> select_finding filter x) filters
  | And filters ->
      List.for_all (fun filter -> select_finding filter x) filters
  | Not filter ->
      not (select_finding filter x)
  | Filter (Affected_type_name name) ->
      let _, names = x in
      List.mem name names
  | Filter Backward ->
      let finding, _ = x in
      (match finding.direction with
       | Backward | Both -> true
       | Forward -> false
      )
  | Filter Forward ->
      let finding, _ = x in
      (match finding.direction with
       | Forward | Both -> true
       | Backward -> false
      )

let compare_files
  ?(filter = And [] (* all *))
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
    Compare.asts options ast1 ast2
  in
  match res with
  | [] -> Ok ()
  | res ->
      let res = List.filter (select_finding filter) res in
      Error (
        match output_format with
        | Text -> Format_text.to_string res
        | JSON -> format_json res
      )
