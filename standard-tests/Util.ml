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
