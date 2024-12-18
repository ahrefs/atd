(*
    Internal Atddiff library used by the 'atddiff' command.
*)

open Printf

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

let check_forgotten_root_types
    ~root_types_superset
    ~path:atd_file
    ~ast_with_inherits =
  match root_types_superset with
  | None -> ()
  | Some root_types_superset ->
      let remaining_types, known_missing_root_types =
        Root_types.check_root_types_superset
          ~root_types_superset
          ~ast_with_inherits
      in
      match remaining_types with
      | [] -> ()
      | _ ->
          let msg =
            sprintf "Some types in file '%s' \
                     were not declared with '--types' or '--ignore'. \
                     You should identify types that are \
                     entry points and add them to '--types' if \
                     you want to check them or to '--ignore' otherwise.\n\
                     The following types are not being checked: %s\n\
                     At least the following types need to be added to '--types' or '--ignore': %s"
              atd_file
              (String.concat ", " remaining_types)
              (match known_missing_root_types with
               | [] -> "<none, check for recursive definitions>"
               | _ -> String.concat ", " known_missing_root_types)
          in
          failwith msg

(*
   To identify properly the dependencies on inherited types, we need
   to take into account dependencies before and after expanding the inherited
   types.
*)

let compare_files
  ?root_types_superset
  ?(filter = And [] (* all *))
  ?(json_defaults_old = false)
  ?(json_defaults_new = false)
  ?(output_format = Text)
  ?(with_locations = true)
    old_file new_file =
  let load_file atd_file =
    let ast_with_inherits =
       atd_file
       |> Atd.Util.load_file ~tags:[]
       |> fst
       |> Atd.Ast.remove_wrap_constructs
    in
    let ast_without_inherits =
      let head, body = ast_with_inherits in
      let body =
        Atd.Inherit.expand_module_body
          ~inherit_fields:true (* simplifies comparison *)
          ~inherit_variants:true (* simplifies comparison *)
          body
      in
      head, body
    in
    ast_with_inherits, ast_without_inherits
  in
  ignore Atd.Inherit.expand_module_body;
  let ast1_with_inherits, ast1_without_inherits = load_file old_file in
  let ast2_with_inherits, ast2_without_inherits = load_file new_file in
  check_forgotten_root_types
    ~root_types_superset
    ~path:old_file
    ~ast_with_inherits:ast1_with_inherits;
  check_forgotten_root_types
    ~root_types_superset
    ~path:new_file
    ~ast_with_inherits:ast2_with_inherits;
  let res =
    let options : Compare.options = {
      json_defaults_old;
      json_defaults_new;
      sort_by = (if with_locations then Location else Hash);
    } in
    Compare.asts options ast1_without_inherits ast2_without_inherits
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
