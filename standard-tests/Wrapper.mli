(**
   A standard test suite parametrized by the details needed to
   build and run programs for the target language.
*)

type json_conf = {
  name: string;

  generate: JSON_tests.json_test -> unit;
  (** Take the ATD file NAME.atd and generate the necessary code in the
      target language. Any exception signals a failure. *)

  compile: JSON_tests.json_test -> unit;
  (** If applicable, compile the generated code into an executable
      that reads JSON from stdin and writes it back to stdout.
      Any exception signals a failure. *)

  run_command: string list;
  (** The command to run the executable. It should read JSON on stdin
      and JSON to stdout. The test framework is in charge of the
      execution. *)

  expected_to_fail: (string * string list option) list;
  (** List of full test names (json_test.name, json_test_case name)
      that are expected to fail. Ideally, this list is empty.

      [(name, None)] indicates that all test cases are expected to fail
      to test [name].

      [(name, Some [a; b])] indicates that only test cases [a] and [b] are
      expected to fail.
  *)
}

(** The standard test suite configured for a specific code generator *)
val make_tests : json_conf -> Testo.t list

(** Helper: runs the command safely and portably, logs the command
    invocation and prints exit status. *)
val run_command : string list -> unit
