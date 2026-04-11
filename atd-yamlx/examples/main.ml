(*
  Minimal example: read an application config from a YAML file and
  pretty-print its contents.

  Build instructions (run from this directory):

    # 1. Generate app_config.ml + app_config.mli from app_config.atd.
    #    atdml is provided by the 'atdml' opam package.
    atdml app_config.atd

    # 2. Compile and run.
    make

  See the Makefile in this directory for the exact compilation command.
*)

open App_config

let show_log_level = function
  | Debug -> "Debug"
  | Info  -> "Info"
  | Warn  -> "Warn"
  | Error -> "Error"

let () =
  let path = "config.yaml" in
  let yaml_text =
    try In_channel.input_all (open_in path)
    with Sys_error msg ->
      Printf.eprintf "Cannot open %s: %s\n%!" path msg;
      exit 1
  in
  (* Step 1: parse YAML into a generic JSON-like tree, preserving locations. *)
  let jsonlike =
    match YAMLx.Values.one_of_yaml ~file:path yaml_text with
    | Error msg ->
        Printf.eprintf "%s\n%!" msg;
        exit 1
    | Ok yaml_val ->
        Atd_yamlx.of_yamlx_value ~path yaml_val
  in
  (* Step 2: deserialize the JSON-like tree into a typed OCaml value.
     Location-aware errors point at the exact line in config.yaml. *)
  let cfg =
    try app_config_of_jsonlike jsonlike
    with Failure msg ->
      Printf.eprintf "%s\n%!" msg;
      exit 1
  in
  Printf.printf "Application : %s\n" cfg.name;
  Printf.printf "Log level   : %s\n" (show_log_level cfg.log_level);
  Printf.printf "Tags        : [%s]\n" (String.concat ", " cfg.tags);
  (match cfg.db with
   | Sqlite file ->
       Printf.printf "Database    : SQLite at %s\n" file
   | Postgres pg ->
       Printf.printf "Database    : PostgreSQL %s@%s:%d (ssl=%b)\n"
         pg.dbname pg.host pg.port pg.ssl)
