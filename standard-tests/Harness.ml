(*
   A standard test harness parametrized by the details needed to
   build and run programs for the target language.
*)

open Printf

module J = JSON_tests

type json_conf = {
  name: string;
  generate: J.json_test -> unit;
  compile: J.json_test -> unit;
  run_command: string list;
  expected_to_fail: (string * string list option) list;
}

let get_cmd_name cmd =
  match cmd with
  | [] -> failwith "bad configuration: empty command"
  | cmd_name :: _ -> cmd_name

(* TODO: add this to Testo? *)
let handle_exit_status cmd (status : Unix.process_status) =
  match status with
  | WEXITED 0 ->
      eprintf "Command exited successfully: %s\n%!"
        (String.concat " " cmd)
  | WEXITED n ->
      ksprintf failwith "Command exited with error code %i: %s"
        n (String.concat " " cmd)
  | WSIGNALED n ->
      ksprintf failwith "Command killed by signal %i: %s"
        n (String.concat " " cmd)
  | WSTOPPED n ->
      ksprintf failwith "Command stopped by signal %i: %s"
        n (String.concat " " cmd)

let make_json_tests (conf : json_conf) =
  JSON_tests.tests
  |> List.map (fun ((test : J.json_test),
                    (standard_outcome : JSON_tests.standard_outcome)) ->
    let expected_to_fail =
      List.assoc_opt test.name conf.expected_to_fail
    in
    test.test_cases
    |> List.map (fun (case : J.json_test_case) ->
      let expected_outcome : Testo.expected_outcome =
        let default : Testo.expected_outcome =
          match standard_outcome with
          | Pass -> Should_succeed
          | Fail -> Should_fail "standard, expected failure"
        in
        match expected_to_fail with
        | None -> default
        | Some None ->
            Should_fail "not implemented"
        | Some (Some names) ->
            if List.mem case.name names then
              Should_fail "not implemented"
            else
              default
      in
      Testo.create
        ~category:["standard"; conf.name; test.name] case.name
        ~expected_outcome
        ~max_duration:15.
        (fun () ->
           (* Work in a temporary directory where we place a copy of the
              ATD file *)
           Testo.with_temp_dir ~chdir:true (fun _cwd ->
             let atd_file_path = Fpath.v ("types.atd") in
             Testo.write_text_file atd_file_path test.atd_defs;
             eprintf "ATD defs in file %s:\n%s\n%!"
               (Fpath.to_string atd_file_path) test.atd_defs;
             conf.generate test;
             conf.compile test;
             let ic, oc as process =
               Util.log_command conf.run_command;
               Unix.open_process_args
                 (get_cmd_name conf.run_command)
                 (Array.of_list conf.run_command) in
             Fun.protect
               ~finally:(fun () ->
                 let status = Unix.close_process process in
                 handle_exit_status conf.run_command status
               )
               (fun () ->
                  output_string oc case.json_input;
                  close_out oc;
                  let yojson_out = Yojson.Safe.from_channel ic in
                  Testo.(check text)
                    (Util.normalize_json case.expected_output)
                    (Util.normalize_yojson yojson_out)
               )
           )
        )
    )
  )
  |> List.flatten

(* We could support tests for more than JSON in the future *)
let make_tests conf =
  make_json_tests conf
