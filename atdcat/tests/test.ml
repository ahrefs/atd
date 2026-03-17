(*
   Test suite for atdcat, using the ATD library directly (no subprocess).

   Each test calls the same ATD functions that atdcat wraps, captures the
   output via Testo's stdout/stderr capture, and compares it against a
   named snapshot in tests/named-snapshots/.

   Run from the atdcat/ directory:
     ./test            -- run all tests
     ./test approve    -- approve new/changed snapshots
*)

(* Convert a test name into a safe file name, the same way atdml does it. *)
let make_filename_from_test_name str =
  String.map (function
    | ' ' -> '_'
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' as c -> c
    | c -> failwith (Printf.sprintf "Invalid character %C in test name %S" c str)
  ) str

let snapshot name =
  Fpath.(v "tests/named-snapshots" / make_filename_from_test_name name)

(* Pretty-print an ATD file to stdout, optionally expanding and/or
   removing wrap constructs. *)
let test_pp test_name
    ?(expand = false)
    ?(inherit_fields = false) ?(inherit_variants = false)
    ?(remove_wraps = false)
    atd_file =
  Testo.create test_name
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(snapshot test_name)
                       ())
    (fun () ->
       let module_ =
         Atd.Util.load_file ~expand ~inherit_fields ~inherit_variants atd_file
       in
       let module_ =
         if remove_wraps then Atd.Ast.remove_wrap_constructs module_
         else module_
       in
       print_string (Atd.Print.to_string (Atd.Ast.Module module_)))

(* Translate an ATD file to JSON Schema and print to stdout. *)
let test_jsonschema test_name ?(xprop = true) ?version atd_file root_type =
  Testo.create test_name
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(snapshot test_name)
                       ())
    (fun () ->
       let module_ =
         Atd.Util.load_file
           ~annot_schema:Atd.Jsonschema.annot_schema
           ~inherit_fields:true ~inherit_variants:true
           atd_file
       in
       Atd.Jsonschema.print ?version ~xprop ~src_name:atd_file ~root_type
         stdout module_)

let tests _env = [
  (* Pretty-printing round-trips *)
  test_pp "roundtrip" "tests/input/test.atd";
  test_pp "roundtrip test2" "tests/input/test2.atd";
  test_pp "soft keywords" "tests/input/soft_keywords.atd";
  test_pp "remove wraps" ~remove_wraps:true "tests/input/test.atd";

  (* JSON Schema output *)
  test_jsonschema "json schema" "tests/input/schema.atd" "root";
  test_jsonschema "json schema no extra fields" ~xprop:false
    "tests/input/schema.atd" "root";
  test_jsonschema "json schema draft-2019-09"
    ~version:Atd.Jsonschema.Draft_2019_09
    "tests/input/schema.atd" "root";

  (* Unused import warning *)
  Testo.create "unused import warning"
    ~checked_output:(Testo.stderr
                       ~expected_stderr_path:(snapshot "unused import warning")
                       ())
    (fun () ->
       (* 'unused_type' is imported but never referenced — a warning must be
          emitted to stderr. 'used_type' is referenced so no warning for it. *)
       ignore (Atd.Util.load_string ~pos_fname:"test_input" {|
from some_module import used_type, unused_type

type t = { x: some_module.used_type }
|}));
]

let () =
  Testo.interpret_argv
    ~project_name:"atdcat"
    tests
