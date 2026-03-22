(*
   The standard test suite for all generators of JSON reader-writers.
*)

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

let tests : json_test list = [
  {
    name = "color_enum";
    atd_defs = {|
type color = [
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
      }
    ]
  }
]
