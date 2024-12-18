(*
    Internal Atddiff library used by the 'atddiff' command.
*)

module T = Atddiff_output_t

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

let rec select_finding filter (x : T.finding) =
  match filter with
  | Or filters ->
      List.exists (fun filter -> select_finding filter x) filters
  | And filters ->
      List.for_all (fun filter -> select_finding filter x) filters
  | Not filter ->
      not (select_finding filter x)
  | Filter (Affected_type_name name) ->
      List.mem name x.affected_types
  | Filter Backward ->
      (match x.direction with
       | Backward | Both -> true
       | Forward -> false
      )
  | Filter Forward ->
      (match x.direction with
       | Forward | Both -> true
       | Backward -> false
      )

let compare_files
  ?(filter = And [] (* all *))
  ?(json_defaults_old = false)
  ?(json_defaults_new = false)
  ?(output_format = Text)
  ?(with_locations = true)
    old_file new_file =
  let load_file atd_file =
    atd_file
    |> Atd.Util.load_file
      ~inherit_fields:true (* simplifies comparison *)
      ~inherit_variants:true (* simplifies comparison *)
      ~tags:[]
    |> fst
    |> Atd.Ast.remove_wrap_constructs
  in
  let ast1 = load_file old_file in
  let ast2 = load_file new_file in
  let res =
    let options : Compare.options = {
      json_defaults_old;
      json_defaults_new;
      sort_by = (if with_locations then Location else Hash);
    } in
    Compare.asts options ast1 ast2
  in
  match res.findings with
  | [] -> Ok ()
  | findings ->
      let res : T.result =
        { findings = List.filter (select_finding filter) findings } in
      Error (
        match output_format with
        | Text -> Format_text.to_string ~with_locations res
        | JSON -> Format_JSON.to_string ~with_locations res ^ "\n"
      )
