(** The standard test suite *)

(** JSON input and expected output matching an ATD type named [t]. *)
type json_test_case = private {
  name: string;
  json_input: string;
  expected_output: string;
}

(** An ATD type specification. It defines a type named [t].
    Once compiled into a program that reads and write JSON data,
    this program will be reused to check multiple test cases.

    A test program is expected to read JSON data of type [t] from stdin
    and write the same data as JSON on stdout.

    The test framework takes care of:
    1. feeding the JSON input to the test program;
    2. capturing the output from stdout;
    3. normalizing the JSON output and comparing it to the expectation.
*)
type json_test = private {
  name: string;
  atd_defs: string;
  test_cases: json_test_case list;
}

val tests : json_test list
