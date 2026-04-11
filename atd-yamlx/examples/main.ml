(*
  Minimal example: read an application config from a YAML file and
  pretty-print its contents.

  Prerequisites (install once via opam):
    opam install atdml atd-yamlx yojson

  Build and run (from this directory):
    dune exec ./main.exe
*)

open Printf
open App_config

let show_log_level = function
  | Debug -> "Debug"
  | Info  -> "Info"
  | Warn  -> "Warn"
  | Error -> "Error"

let () =
  let file = "config.yaml" in
  (* Step 1: parse YAML into a generic JSON-like tree, preserving locations. *)
  let jsonlike =
    match YAMLx.Values.one_of_yaml_file file with
    | Error msg -> eprintf "%s\n%!" msg; exit 1
    | Ok yaml_val ->
        match Atd_yamlx.of_yamlx_value ~file yaml_val with
        | Error msg -> eprintf "%s\n%!" msg; exit 1
        | Ok tree -> tree
  in
  (* Step 2: deserialize the JSON-like tree into a typed OCaml value.
     Location-aware errors point at the exact line in config.yaml. *)
  let cfg =
    try app_config_of_jsonlike jsonlike
    with Failure msg ->
      eprintf "%s\n%!" msg;
      exit 1
  in
  printf "Application : %s\n" cfg.name;
  printf "Log level   : %s\n" (show_log_level cfg.log_level);
  printf "Tags        : [%s]\n" (String.concat ", " cfg.tags);
  (match cfg.db with
   | Sqlite file ->
       printf "Database    : SQLite at %s\n" file
   | Postgres pg ->
       printf "Database    : PostgreSQL %s@%s:%d (ssl=%b)\n"
         pg.dbname pg.host pg.port pg.ssl)
