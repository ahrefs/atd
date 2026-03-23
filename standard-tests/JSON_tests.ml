(*
   The standard test suite for all generators of JSON reader-writers.

   Two lists of tests are defined:
   - passing tests
   - failing tests

   Each test consists of one ATD spec and multiple JSON inputs paired with
   the expected output.
*)

open Printf

type json_test_case = {
  name: string;
  json_input: string;
  expected_output: string;
}

type json_test = {
  name: string;
  atd_defs: string;
  test_cases: json_test_case list;
}

type standard_outcome = Pass | Fail

let passing_tests : json_test list = [
  {
    name = "color_enum";
    atd_defs = {|
type t = [
  | Red
  | Green <json name="green">
  | Blue
]
|};
    test_cases = [
      {
        name = "default name";
        json_input = {|"Red"|};
        expected_output = {|"Red"|};
      };
      {
        name = "json name";
        json_input = {|"green"|};
        expected_output = {|"green"|};
      };
      {
        name = "Blue";
        json_input = {|"Blue"|};
        expected_output = {|"Blue"|};
      };
    ]
  };

  {
    name = "type_aliases";
    atd_defs = {|
type id = string
type score = float
type tag_list = string list
type opt_name = string option

type t = {
  id: id;
  score: score;
  tags: tag_list;
  name: opt_name;
}
|};
    test_cases = [
      {
        name = "Some name";
        json_input = {|{"id": "abc", "score": 1.5, "tags": ["a", "b"], "name": ["Some", "x"]}|};
        expected_output = {|{"id": "abc", "score": 1.5, "tags": ["a", "b"], "name": ["Some", "x"]}|};
      };
      {
        name = "None name";
        json_input = {|{"id": "xyz", "score": 0.5, "tags": [], "name": "None"}|};
        expected_output = {|{"id": "xyz", "score": 0.5, "tags": [], "name": "None"}|};
      };
    ]
  };

  {
    name = "sum_types";
    atd_defs = {|
type shape = [
  | Circle of float
  | Rect <json name="rectangle"> of (float * float)
  | Dot
  | Arc <json name="arc"> of float
]

type t = shape list
|};
    test_cases = [
      {
        name = "Circle";
        json_input = {|[["Circle", 1.5]]|};
        expected_output = {|[["Circle", 1.5]]|};
      };
      {
        name = "Rect with json name";
        json_input = {|[["rectangle", [1.5, 3.5]]]|};
        expected_output = {|[["rectangle", [1.5, 3.5]]]|};
      };
      {
        name = "unit constructor";
        json_input = {|["Dot"]|};
        expected_output = {|["Dot"]|};
      };
      {
        name = "Arc with json name";
        json_input = {|[["arc", 3.5]]|};
        expected_output = {|[["arc", 3.5]]|};
      };
      {
        name = "all shapes";
        json_input = {|[["Circle", 1.5], ["rectangle", [1.5, 3.5]], "Dot", ["arc", 3.5]]|};
        expected_output = {|[["Circle", 1.5], ["rectangle", [1.5, 3.5]], "Dot", ["arc", 3.5]]|};
      };
    ]
  };

  {
    name = "records";
    atd_defs = {|
type t = {
  name: string;
  age: int;
  x: int nullable;
  ?email: string option;
  address <json name="addr">: string;
}
|};
    test_cases = [
      {
        name = "no optional fields";
        json_input = {|{"name": "X", "age": 42, "x": null, "addr": "home"}|};
        expected_output = {|{"name": "X", "age": 42, "x": null, "addr": "home"}|};
      };
      {
        name = "nullable x present";
        json_input = {|{"name": "X", "age": 42, "x": 7, "addr": "home"}|};
        expected_output = {|{"name": "X", "age": 42, "x": 7, "addr": "home"}|};
      };
      {
        name = "with optional email";
        json_input = {|{"name": "Y", "age": 25, "x": null, "email": "y@example.com", "addr": "away"}|};
        expected_output = {|{"name": "Y", "age": 25, "x": null, "email": "y@example.com", "addr": "away"}|};
      };
    ]
  };

  {
    name = "builtin_types";
    atd_defs = {|
type t = {
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
|};
    test_cases = [
      {
        name = "all builtin types";
        json_input = {|{
  "a_unit": null,
  "a_bool": false,
  "a_int": -2,
  "a_float": 1.5,
  "a_string": "x y",
  "a_list": [1, 2, 3],
  "a_option": "None",
  "a_nullable": null,
  "a_abstract": {"key": "val"},
  "a_tuple": [12, "ddd", true],
  "a_nested": ["Some", [[1.0, 1.5]]]
}|};
        expected_output = {|{
  "a_unit": null,
  "a_bool": false,
  "a_int": -2,
  "a_float": 1.5,
  "a_string": "x y",
  "a_list": [1, 2, 3],
  "a_option": "None",
  "a_nullable": null,
  "a_abstract": {"key": "val"},
  "a_tuple": [12, "ddd", true],
  "a_nested": ["Some", [[1.0, 1.5]]]
}|};
      };
    ]
  };

  {
    name = "parametric_types";
    atd_defs = {|
type 'a result = [
  | Ok of 'a
  | Error of string
]

type ('a, 'b) either = [
  | Left of 'a
  | Right of 'b
]

type t = (int result * (bool, string) either)
|};
    test_cases = [
      {
        name = "Ok and Right";
        json_input = {|[["Ok", 12], ["Right", "a"]]|};
        expected_output = {|[["Ok", 12], ["Right", "a"]]|};
      };
      {
        name = "Error and Left";
        json_input = {|[["Error", "oops"], ["Left", true]]|};
        expected_output = {|[["Error", "oops"], ["Left", true]]|};
      };
    ]
  };

  {
    name = "json_repr_object";
    atd_defs = {|
type string_map = (string * int) list <json repr="object">

type nested = (string * string list) list <json repr="object">

type t = {
  counts: string_map;
  tags: nested;
}
|};
    test_cases = [
      {
        name = "string_map and nested";
        json_input = {|{"counts": {"a": 1, "b": 2}, "tags": {"x": ["p", "q"]}}|};
        expected_output = {|{"counts": {"a": 1, "b": 2}, "tags": {"x": ["p", "q"]}}|};
      };
    ]
  };

  {
    name = "mutually_recursive";
    (* Use 'tree' + 'type t = tree' alias so that 'node.children: (tree * tree)'
       doesn't conflict with the submodule-local 'type t = node' that backends
       may introduce per their module conventions. *)
    atd_defs = {|
type tree = [
  | Leaf
  | Node of node
]

type node = {
  value: int;
  children: (tree * tree);
}

type t = tree
|};
    test_cases = [
      {
        name = "Leaf";
        json_input = {|"Leaf"|};
        expected_output = {|"Leaf"|};
      };
      {
        name = "Node";
        json_input = {|["Node", {"value": 0, "children": ["Leaf", "Leaf"]}]|};
        expected_output = {|["Node", {"value": 0, "children": ["Leaf", "Leaf"]}]|};
      };
    ]
  };
]

(*
   All these tests are expected to fail with an exception.
*)
let failing_tests : json_test list = [
  {
    name = "invalid_atd_syntax";
    atd_defs = {|!!!|};
    test_cases = [
      {
        name = "dummy";
        json_input = {|[]|};
        expected_output = {|[]|};
      };
    ]
  };
  {
    name = "invalid_json_syntax";
    atd_defs = {|type t = int list|};
    test_cases = [
      {
        name = "empty input";
        json_input = {||};
        expected_output = {|[]|};
      };
    ]
  };
  {
    name = "json_type_error";
    atd_defs = {|type t = int list|};
    test_cases = [
      {
        name = "number instead of array";
        json_input = {|42|};
        expected_output = {|[]|};
      };
    ]
  }
]

(* Reimplement String.for_all because it's not available in OCaml 4.08 *)
let string_for_all func str =
  try
    String.iter (fun c -> if not (func c) then raise Exit) str;
    true
  with Exit -> false

(* Sanity check for names that may become part of file names. *)
let check_safe_basename str =
  if not (str <> "" &&
          string_for_all (function
            | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
            | _ -> false
          ) str
         )
  then
    ksprintf failwith "invalid name in test suite: %S" str

(* Sanity check for test names. *)
let check_test_name str =
  if not (str <> "" &&
          string_for_all (function
            | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9'
            | ' ' | '-' -> true
            | _ -> false
          ) str
         )
  then
    ksprintf failwith "invalid name in test suite: %S" str

let check_json_test (x : json_test) =
  check_safe_basename x.name;
  List.iter (fun (x : json_test_case) -> check_test_name x.name) x.test_cases

let check_tests xs =
  List.iter check_json_test xs

let tests =
  check_tests passing_tests;
  check_tests failing_tests;
  List.map (fun x -> (x, Pass)) passing_tests
  @ List.map (fun x -> (x, Fail)) failing_tests
