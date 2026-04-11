(*
   Unit tests for atd-yamlx.

   Run from the project root:
     dune runtest atd-yamlx
*)

open Atd_jsonlike

(* A Testo.testable for AST.t: equality ignores locations, show omits them. *)
let ast_t : AST.t Testo.testable =
  Testo.testable AST.show AST.equal

(* ===== Helpers ===== *)

let parse_one ?(path = "test") yaml_str =
  match YAMLx.Values.of_yaml ~file:path yaml_str with
  | Error msg -> failwith ("YAMLx parse error: " ^ msg)
  | Ok [v]    -> Atd_yamlx.of_yamlx_value_exn ~path v
  | Ok _      -> failwith "expected exactly one YAML document"

(* ===== Tests ===== *)

(* Basic round-trip: parse a YAML document and compare the resulting
   jsonlike tree against an expected value (locations ignored). *)
let test_basic_document () =
  let result = parse_one {|
answer: 42
flag: true
greeting: "hello"
nothing: null
scores:
  - 1
  - 2
  - 3
|}
  in
  let expected =
    let no_loc = Loc.{ start = Pos.{row=0;column=0}; end_ = Pos.{row=0;column=0}; path=None } in
    AST.Object (no_loc, [
      (no_loc, "answer",   AST.Number (no_loc, Number.of_int 42));
      (no_loc, "flag",     AST.Bool   (no_loc, true));
      (no_loc, "greeting", AST.String (no_loc, "hello"));
      (no_loc, "nothing",  AST.Null    no_loc);
      (no_loc, "scores",   AST.Array  (no_loc, [
        AST.Number (no_loc, Number.of_int 1);
        AST.Number (no_loc, Number.of_int 2);
        AST.Number (no_loc, Number.of_int 3);
      ]));
    ])
  in
  Testo.check ast_t expected result

(* Floats, nested maps, and an empty sequence. *)
let test_nested () =
  let result = parse_one {|
ratio: 1.5
inner:
  x: 0
  y: 0
tags: []
|}
  in
  let no_loc = Loc.{ start = Pos.{row=0;column=0}; end_ = Pos.{row=0;column=0}; path=None } in
  let expected =
    AST.Object (no_loc, [
      (no_loc, "ratio", AST.Number (no_loc, Number.of_float 1.5));
      (no_loc, "inner", AST.Object (no_loc, [
        (no_loc, "x", AST.Number (no_loc, Number.of_int 0));
        (no_loc, "y", AST.Number (no_loc, Number.of_int 0));
      ]));
      (no_loc, "tags", AST.Array (no_loc, []));
    ])
  in
  Testo.check ast_t expected result

(* A non-string map key should return an Error. *)
let test_non_string_key_error () =
  let yaml_str = {|
? 42
: value
|}
  in
  match YAMLx.Values.of_yaml yaml_str with
  | Error msg -> failwith ("YAMLx parse error: " ^ msg)
  | Ok [v] ->
      (match Atd_yamlx.of_yamlx_value v with
       | Error _ -> ()
       | Ok _ -> failwith "expected an error for non-string map key")
  | Ok _ -> failwith "expected exactly one YAML document"

(* ===== Test list ===== *)

let tests _env = [
  Testo.create "basic document: scalars, bool, null, sequence" test_basic_document;
  Testo.create "nested maps, float, empty sequence"            test_nested;
  Testo.create "non-string map key returns Error"              test_non_string_key_error;
]

let () =
  Testo.interpret_argv
    ~project_name:"atd-yamlx"
    tests
