(*
   Test suite for atdml code generation.

   Each test writes ATD source to a temp file, runs the code generator, and
   captures both the generated .mli and .ml as a snapshot.
*)

open Printf

let ( // ) = Fpath.( // )

(* Require 'dune install --prefix local' to have executables under the project
   root in local/bin *)
let get_atdml_command () =
  Fpath.v (Sys.getcwd ()) // Fpath.v "../local/bin/atdml"

let atdml = get_atdml_command () |> Fpath.to_string

let run_command args =
  let shell_command =
    (* Escape the arguments using the OCaml conventions rather than
       the local shell's conventions.
       This should be good enough for testing. *)
    args
    |> List.map (sprintf "%S")
    |> String.concat " "
  in
  eprintf "CWD %s\n" (Sys.getcwd ());
  eprintf "RUN %s\n" shell_command;
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

(* Run the code generator on [atd_src] and return (mli_content, ml_content).
   Uses a temp directory so as not to pollute the working directory. *)
let run_codegen ~test_name ~file_name atd_src =
  Testo.with_temp_dir ~chdir:true (fun _dir ->
    let atd_file = file_name ^ ".atd" in
    Testo.write_text_file (Fpath.v atd_file) atd_src;
    run_command [atdml; atd_file];
    let mli = Testo.read_text_file (Fpath.v (file_name ^ ".mli")) in
    let ml = Testo.read_text_file (Fpath.v (file_name ^ ".ml"))  in
    (mli, ml)
  )

(* Create a test program that reads JSON and writes it back.
   JSON data is printed so it can be captured by the test framework. *)
let test_program ~type_name ~json_in = sprintf {|
let () =
  let json_in = %S in
  let yojson_in = Yojson.Safe.from_string json_in in
  let typed_data = Types.%s_of_yojson yojson_in in
  let yojson_out = Types.yojson_of_%s typed_data in
  let json_out = Yojson.Safe.pretty_to_string yojson_out in
  let json_in_comparable = Yojson.Safe.pretty_to_string yojson_in in
  Printf.printf "--- Input:\n%%s\n" json_in_comparable;
  Printf.printf "--- Output:\n%%s\n" json_out
|}
    json_in type_name type_name

let build_and_run ~mli ~ml ~type_name ~json_in =
  Testo.with_temp_dir ~chdir:true (fun _dir ->
    Testo.write_text_file (Fpath.v "Types.mli") mli;
    Testo.write_text_file (Fpath.v "Types.ml") ml;
    Testo.write_text_file
      (Fpath.v "Main.ml")
      (test_program ~type_name ~json_in);
    let build_cmd =
      [ "ocamlfind"; "opt"; "-o"; "test_atdml"; "Types.mli"; "Types.ml";
        "Main.ml"; "-package"; "yojson"; "-linkpkg" ]
    in
    let run_cmd = ["./test_atdml"] in
    run_command build_cmd;
    run_command run_cmd
  )

(* Convert test name into a safe file name *)
let make_filename_from_test_name str =
  String.map (function
    | ' ' -> '_'
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' as c -> c
    | c -> failwith (sprintf "Invalid character %C in test name %S" c str)
  )
    str

(* Run an end-to-end test:
   - invoke atdml to translate an ATD file into OCaml
   - compile it into a program that reads and writes JSON of a designated type
   - run it to check that the conversions from JSON data to Yojson and back
     work as intended
   The interesting outputs (mli, ml, JSON) are printed to stdout and
   captured as a test snapshot.
*)
let test_e2e test_name ~atd_src ~type_name ~json_in =
  let file_name = make_filename_from_test_name test_name in
  Testo.create test_name
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(
                         Fpath.(v "tests/named-snapshots" / file_name))
                       ())
    (fun () ->
      let mli, ml = run_codegen ~test_name ~file_name atd_src in
      print_string mli;
      print_string "--- ml ---\n";
      print_string ml;
      build_and_run ~mli ~ml ~type_name ~json_in
    )

let tests _env = [
  Testo.create "run atdml from temp folder"
    (fun () ->
       (* Avoid relying on 'dune exec' which is tricky *)
       Testo.with_temp_dir ~chdir:true (fun _dir ->
         run_command [atdml; "--help"]
       )
    );

  test_e2e "color enum"
    ~atd_src:{|
type color = [
  | Red
  | Green <json name="green">
  | Blue
]

type colors = (color * color * color)
|}
    ~type_name:"colors"
    ~json_in:{|
[
  "Red", "green", "Blue"
]
|}
  ;

  test_e2e "type aliases"
    ~atd_src:{|
type id = string
type score = float
type tag_list = string list
type opt_name = string option

type all = {
  id: id;
  score: score;
  tags: tag_list;
  name: opt_name;
}
|}
    ~type_name:"all"
    ~json_in:{|
{
  "id": "abc",
  "score": 1.23,
  "tags": ["a", "b"],
  "name": ["Some", "x"]
}
|}
;

  test_e2e "classic sum types"
    ~atd_src:{|
type shape = [
  | Circle of float
  | Rect <json name="rectangle"> of (float * float)
  | Dot <ocaml name="Point">
  | Arc <json name="arc"> <ocaml name="ArcShape"> of float
]

type shapes = shape list
|}
    ~type_name:"shapes"
    ~json_in:{|
[
  [ "Circle", 1 ],
  [ "rectangle", [1.2, 3]],
  "Dot",
  [ "arc", 3.0 ]
]
|}
;

  test_e2e "polymorphic variants"
    ~atd_src:{|
type status = [
  | Active
  | Inactive
  | Pending of string
] <ocaml repr="poly">

type statuses = status list
|}
    ~type_name:"statuses"
    ~json_in:{|
[
  "Active", [ "Pending", "abc" ]
]
|}
;

  test_e2e "records"
    ~atd_src:{|
type person = {
  name: string;
  age: int;
  lol: int nullable;
  ?email: string option;
  ~score: float;
  ~active: bool;
  ~tags: string list;
  ~level <ocaml default="1">: int;
  address <json name="addr">: string;
}
|}
    ~type_name:"person"
    ~json_in:{|
{
  "name": "X",
  "age": 42,
  "lol": null,
  "addr": "xxxx"
}
|}
;

  test_e2e "builtin types"
    ~atd_src:{|
type all_types = {
  a_unit: unit;
  a_bool: bool;
  a_int: int;
  a_float: float;
  a_string: string;
  a_list: int list;
  a_option: string option;
  a_nullable: bool nullable;
  a_abstract: abstract;
  a_tuple: (int * string * bool);
  a_nested: (float list) option;
}
|}
    ~type_name:"all_types"
    ~json_in:{|
{
  "a_unit": null,
  "a_bool": false,
  "a_int": -2,
  "a_float": 9.6,
  "a_string": "x y",
  "a_list": [1,2,3],
  "a_option": "None",
  "a_nullable": null,
  "a_abstract": {"key": "val"},
  "a_tuple": [ 12, "ddd", true ],
  "a_nested": [ "Some", [[1, 2.3]] ]
}
|}
;

  test_e2e "parametric types"
    ~atd_src:{|
type 'a result = [
  | Ok of 'a
  | Error of string
]

type ('a, 'b) either = [
  | Left of 'a
  | Right of 'b
]

type all = (int result * (bool, string) either)
|}
    ~type_name:"all"
    ~json_in:{|
[
  [ "Ok", 12 ],
  [ "Right", "a" ]
]
|}
;

  test_e2e "mutually recursive types"
    ~atd_src:{|
type tree = [
  | Leaf
  | Node of node
]

type node = {
  value: int;
  children: (tree * tree);
}
|}
    ~type_name:"tree"
    ~json_in:{|
  [ "Node", { "value": 0, "children": [ "Leaf", "Leaf" ] } ]
|}
;
]

let () =
  Testo.interpret_argv
    ~project_name:"atdml"
    tests
