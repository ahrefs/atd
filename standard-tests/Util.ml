(*
   Utilities for running tests from OCaml
*)

open Printf

let log_shell_command shell_command =
  eprintf "CWD %s\n" (Sys.getcwd ());
  eprintf "RUN %s\n" shell_command;
  flush stderr

(* This is meant to quote the arguments according to the local
   shell (sh on Unix, cmd.exe on Win32) *)
let make_shell_command args =
  args
  |> List.map Filename.quote
  |> String.concat " "

let log_command args =
  args
  |> make_shell_command
  |> log_shell_command

let run_command cmd =
  (* Unix.open_process_args would be preferable because it doesn't
     require a shell which comes with escaping issues but it's
     broken on OCaml 4.08 and some later versions (which ones?),
     failing to consult the PATH environment variable on Unix systems.

     See https://github.com/ocaml/ocaml/pull/10084

  let oc = Unix.open_process_args_out (get_cmd_name cmd) (Array.of_list cmd) in
  close_out oc;
  Unix.close_process_out oc |> handle_exit_status cmd
  *)
  let shell_command = make_shell_command cmd in
  log_shell_command shell_command;
  flush stdout;
  flush stderr;
  match Sys.command shell_command with
  | 0 -> ()
  | n ->
      let msg =
        sprintf "Error: shell command failed with code %i: %s\n"
          n shell_command
      in
      failwith msg

(* Sort the key/value pairs by key, alphabetically *)
let rec normalize_json_tree (x : Yojson.Safe.t) =
  match x with
  | `Null
  | `Bool _
  | `Int _
  | `Intlit _
  | `Float _
  | `String _ as x -> x
  | `Assoc xs ->
      `Assoc (
        xs
        |> List.map (fun (k, v) -> (k, normalize_json_tree v))
        |> List.stable_sort (fun (a, _) (b, _) -> String.compare a b)
      )
  | `List xs -> `List (List.map normalize_json_tree xs)

(* List.iter + indicate if the current element is the last element *)
let rec iter func xs =
  match xs with
  | [] -> ()
  | [x] -> func x true
  | x :: xs -> func x false; iter func xs

let format_yojson_naively (x : Yojson.Safe.t) =
  (*
     indent: a string of spaces to print at the beginning of each line
     suffix: a string to append at the end of the value, preferably
             on the same line
  *)
  let rec pp indent buf suffix = function
    | `Null
    | `Bool _
    | `Int _
    | `Intlit _
    | `Float _
    | `String _ as x ->
        bprintf buf "%s%s%s\n" indent (Yojson.Safe.to_string x) suffix
    | `Assoc [] ->
        bprintf buf "%s{}%s\n" indent suffix;
    | `Assoc xs ->
        bprintf buf "%s{\n" indent;
        iter (fun (k, v) is_last ->
          let indent = "  " ^ indent in
          bprintf buf "%s%s:\n" indent (Yojson.Safe.to_string (`String k));
          pp ("  " ^ indent) buf (if is_last then "" else ",") v
        ) xs;
        bprintf buf "%s}%s\n" indent suffix
  | `List [] ->
      bprintf buf "%s[]%s\n" indent suffix;
  | `List xs ->
      bprintf buf "%s[\n" indent;
      iter (fun v is_last ->
        pp ("  " ^ indent) buf (if is_last then "" else ",") v
      ) xs;
      bprintf buf "%s]%s\n" indent suffix
  in
  let buf = Buffer.create 100 in
  pp "" buf "" x;
  Buffer.contents buf

let format_json_naively json =
  json
  |> Yojson.Safe.from_string
  |> format_yojson_naively

let normalize_yojson yojson =
  yojson
  |> normalize_json_tree
  |> format_yojson_naively

let normalize_json json =
  json
  |> Yojson.Safe.from_string
  |> normalize_yojson
