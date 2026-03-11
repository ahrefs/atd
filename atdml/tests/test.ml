(*
   Test suite for atdml code generation.

   Each test writes ATD source to a temp file, runs the code generator, and
   captures both the generated .mli and .ml as a snapshot.
*)

open Printf

let ( // ) = Fpath.( // )

(* Find the atdml binary.
   Run 'make' from /atdml/ to install it at '../local/bin/atdml'.
   See CONTRIBUTING.md for the recommended workflow. *)
let atdml =
  (Fpath.v (Sys.getcwd ()) // Fpath.v "../local/bin/atdml")
  |> Fpath.to_string

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
   JSON data is printed so it can be captured by the test framework.

   type_name: the OCaml type name, not the ATD type name (in case they differ)
*)
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

let build_and_run ?(extra_sources = []) ?(extra_atd_files = []) ~mli ~ml ~type_name ~json_in () =
  Testo.with_temp_dir ~chdir:true (fun _dir ->
    List.iter (fun (fname, content) ->
      Testo.write_text_file (Fpath.v fname) content
    ) extra_sources;
    (* Generate OCaml files from extra ATD files (e.g. imported modules).
       Each "foo.atd" produces "foo.mli" and "foo.ml" via atdml. *)
    let extra_ocaml_files =
      List.concat (List.map (fun (atd_fname, atd_src) ->
        let base = Filename.chop_suffix atd_fname ".atd" in
        Testo.write_text_file (Fpath.v atd_fname) atd_src;
        run_command [atdml; atd_fname];
        [ base ^ ".mli"; base ^ ".ml" ]
      ) extra_atd_files)
    in
    Testo.write_text_file (Fpath.v "Types.mli") mli;
    Testo.write_text_file (Fpath.v "Types.ml") ml;
    Testo.write_text_file
      (Fpath.v "Main.ml")
      (test_program ~type_name ~json_in);
    let extra_files = List.map fst extra_sources in
    let build_cmd =
      [ "ocamlfind"; "opt"; "-o"; "test_atdml" ]
      @ extra_files
      @ extra_ocaml_files
      @ [ "Types.mli"; "Types.ml"; "Main.ml"; "-package"; "yojson"; "-linkpkg" ]
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

(* Run a test that only checks the generated .mli and .ml output (no
   compilation), useful when the generated code uses ppx attributes that
   require libraries not available in the test environment. *)
let test_codegen_snapshot test_name ~atd_src =
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
      print_string ml
    )

(* Run an end-to-end test that expects the code generator to fail.
   The error message (without the location prefix) is captured as a snapshot. *)
let _test_codegen_error test_name ~atd_src =
  let file_name = make_filename_from_test_name test_name in
  Testo.create test_name
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(
                         Fpath.(v "tests/named-snapshots" / file_name))
                       ())
    (fun () ->
      Testo.with_temp_dir ~chdir:true (fun _dir ->
        let atd_file = file_name ^ ".atd" in
        Testo.write_text_file (Fpath.v atd_file) atd_src;
        match
          (try Atdml.Codegen.run_file atd_file; `Ok
           with
           | Atd.Ast.Atd_error msg -> `Error msg
           | Failure msg -> `Error msg)
        with
        | `Ok -> failwith "Expected code generation to fail but it succeeded"
        | `Error msg ->
            (* The message starts with a location "path, line X, chars Y-Z:\n".
               Strip that prefix so the snapshot is stable across runs. *)
            let body =
              match String.split_on_char '\n' msg with
              | _ :: rest when rest <> [] -> String.concat "\n" rest
              | _ -> msg
            in
            print_endline body
      )
    )

(* Run an end-to-end test:
   - invoke atdml to translate an ATD file into OCaml
   - compile it into a program that reads and writes JSON of a designated type
   - run it to check that the conversions from JSON data to Yojson and back
     work as intended
   The interesting outputs (mli, ml, JSON) are printed to stdout and
   captured as a test snapshot.
*)
let test_e2e ?(extra_sources = []) ?(extra_atd_files = []) test_name ~atd_src ~type_name ~json_in =
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
      build_and_run ~extra_sources ~extra_atd_files ~mli ~ml ~type_name ~json_in ()
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

  test_e2e "type name t"
    ~atd_src:{|
type t = int
|}
    ~type_name:"t"
    ~json_in:"42"
  ;

  test_e2e "type name yojson"
    ~atd_src:{|
type yojson = int
|}
    ~type_name:"yojson_"
    ~json_in:"42"
  ;

  test_e2e "type name json"
    ~atd_src:{|
type json = string
|}
    ~type_name:"json_"
    ~json_in:{|"hello"|}
  ;

  test_e2e "type name module"
    ~atd_src:{|
type module__ = bool
type module_1 = string list
type module = int
type module_ = string
|}
    ~type_name:"module_" (* ATD type name "module" -> int *)
    ~json_in:"42"
  ;

  test_e2e "adapter"
    (* The adapter converts between ATD's ["Constructor", ...] sum encoding
       and an object with a "type" field, i.e. {"type": "Image", "url": "..."}.
       We use adapter.to_ocaml / adapter.from_ocaml with inline expressions. *)
    ~extra_sources:[
      ("My_adapter.ml", {|
(* Converts {"type": "Foo", ...rest} <-> ["Foo", {...rest}] *)
let normalize = function
  | `Assoc fields ->
      let tag =
        match List.assoc_opt "type" fields with
        | Some (`String s) -> s
        | _ -> failwith "My_adapter.normalize: missing 'type' field"
      in
      let rest = List.filter (fun (k, _) -> k <> "type") fields in
      `List [`String tag; `Assoc rest]
  | x -> x

let restore = function
  | `List [`String tag; `Assoc rest] ->
      `Assoc (("type", `String tag) :: rest)
  | `String tag ->
      `Assoc [("type", `String tag)]
  | x -> x
|})]
    ~atd_src:{|
type image = { url: string }
type text = { title: string; body: string }

type document = [
  | Image of image
  | Text of text
] <json adapter.ocaml="My_adapter">
|}
    ~type_name:"document"
    ~json_in:{|{"type": "Image", "url": "https://example.com/pic.jpg"}|}
  ;

  test_codegen_snapshot "attr"
    ~atd_src:{|
type point <ocaml attr="deriving show"> = {
  x: float;
  y: float;
}

type points <ocaml attr="deriving show"> = point list
|}
  ;

  test_codegen_snapshot "doc"
    ~atd_src:{|
<doc text="Module-level documentation.">

type color <doc text="A color value."> = [
  | Red   <doc text="The color red.">
  | Green <doc text="The color green.">
  | Blue
]

type person <doc text="A person record.

Second paragraph."> = {
  name <doc text="The person's name.">: string;
  age  <doc text="Age in years.">: int;
  ?email: string option;
}

type alias <doc text="An alias for string."> = string

(* Mutually recursive types with doc annotations, to verify
   that (** comment *) before 'and' is rendered correctly. *)
type tree <doc text="A binary tree."> = [
  | Leaf
  | Node of node
]

type node <doc text="An internal tree node."> = {
  value: int;
  left: tree;
  right: tree;
}
|}
  ;

  test_e2e "wrap"
    ~atd_src:{|
(* wrap with explicit t/wrap/unwrap: string in JSON, int in OCaml *)
type uid = string wrap <ocaml t="int" wrap="int_of_string" unwrap="string_of_int">

(* wrap without annotation: identity, behaves like a plain alias *)
type tag = string wrap

type record = {
  id: uid;
  label: tag;
}
|}
    ~type_name:"record"
    ~json_in:{|{"id": "42", "label": "hello"}|}
  ;

  test_codegen_snapshot "keyword field and variant names"
    (* Fields named after OCaml keywords must be auto-renamed (e.g. 'type' ->
       'type_').  Variant constructors are always capitalized so they cannot
       conflict with lowercase keywords; the vtr layer is still exercised. *)
    ~atd_src:{|
type record_with_keyword_fields = {
  method: string;
  object: int;
  ?begin: bool option;
  ~end: string list;
}

type sum_with_keyword_names = [
  | Fun
  | Method of string
]
|}
  ;

  test_e2e "json repr object"
    ~atd_src:{|
type string_map = (string * int) list <json repr="object">

type nested = (string * string list) list <json repr="object">

type record = {
  counts: string_map;
  tags: nested;
}
|}
    ~type_name:"record"
    ~json_in:{|{"counts": {"a": 1, "b": 2}, "tags": {"x": ["p", "q"]}}|}
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

  (* End-to-end test for imports:
     - 'import base_types' uses extra_atd_files: atdml generates base_types.ml
       from an ATD source, producing module Base_types (no alias needed).
     - 'import long.module.path as ext' uses extra_sources: a hand-written
       Path module that mimics what the generated code expects.  The .ml file
       will contain 'module Ext = Path' and use Ext throughout; the .mli uses
       Path directly. *)
  test_e2e "imports e2e"
    ~extra_atd_files:[
      ("base_types.atd", {|
type person_name = string
type label = string
|})]
    ~extra_sources:[
      (* 'import long.module.path as ext' resolves to OCaml module
         Long.Module.Path, so we provide Long.ml with that nested structure. *)
      ("Long.mli", {|
module Module : sig
  module Path : sig
    type tag = string
    val tag_of_yojson : Yojson.Safe.t -> tag
    val yojson_of_tag : tag -> Yojson.Safe.t
  end
end
|});
      ("Long.ml", {|
module Module = struct
  module Path = struct
    type tag = string

    let tag_of_yojson : Yojson.Safe.t -> tag = function
      | `String s -> s
      | x -> failwith ("Long.Module.Path.tag_of_yojson: " ^ Yojson.Safe.to_string x)

    let yojson_of_tag (s : tag) : Yojson.Safe.t = `String s
  end
end
|})]
    ~atd_src:{|
import base_types
import long.module.path as ext

type item = {
  name: base_types.person_name;
  tag: ext.tag;
}
|}
    ~type_name:"item"
    ~json_in:{|{"name": "Alice", "tag": "hello"}|}
  ;

  test_e2e "field prefix"
    ~atd_src:{|
(* 'ule' + prefix "mod" = "module" (keyword) → field name "module_", label "ule"
   'if' (keyword) + prefix "mod" = "modif" (not a keyword) → field name "modif", label "if_" *)
type t = {
  ule: int;
  if: int;
} <ocaml field_prefix="mod">
|}
    ~type_name:"t"
    ~json_in:{|{"ule": 1, "if": 2}|}
  ;

  (* Snapshot test only: verify that qualified type names (imports) produce
     correct OCaml module references. *)
  test_codegen_snapshot "imports"
    ~atd_src:{|
import base_types

import long.module.path as ext

type greeting = {
  name: base_types.person_name;
  message: string;
}

type tagged = {
  value: ext.tag;
  label: base_types.label;
}
|};
]

let () =
  Testo.interpret_argv
    ~project_name:"atdml"
    tests
