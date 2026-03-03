(*
   Test suite for atdml code generation.

   Each test writes ATD source to a temp file, runs the code generator, and
   captures both the generated .mli and .ml as a snapshot.
*)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* Run the code generator on [atd_src] and return (mli_content, ml_content).
   Uses a temp directory so as not to pollute the working directory. *)
let run_codegen ~test_name ~file_name atd_src =
  Testo.with_temp_dir ~chdir:true (fun tmpdir ->
    let atd_file = Fpath.add_seg tmpdir (file_name ^ ".atd") in
    let oc = open_out (Fpath.to_string atd_file) in
    output_string oc atd_src;
    close_out oc;
    Atdml.Codegen.run_file (Fpath.to_string atd_file);
    let mli = read_file (file_name ^ ".mli") in
    let ml  = read_file (file_name ^ ".ml")  in
    (mli, ml)
  )

let make_filename_from_test_name str =
  String.concat "_" (String.split_on_char ' ' str)

let test test_name atd_src =
  let file_name = make_filename_from_test_name test_name in
  Testo.create test_name
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(Fpath.(v "tests/named-snapshots"
                                                        / file_name))
                       ())
    (fun () ->
      let mli, ml = run_codegen ~test_name ~file_name atd_src in
      print_string mli;
      print_string "--- ml ---\n";
      print_string ml)

let tests _env = [

  test "color enum"
    {|type color = [
  | Red
  | Green <json name="green">
  | Blue
]
|};

  test "type aliases"
    {|type id = string
type score = float
type tag_list = string list
type opt_name = string option
|};

  test "classic sum types"
    {|type direction = [
  | North
  | South
  | East
  | West
]

type shape = [
  | Circle of float
  | Rect <json name="rectangle"> of (float * float)
  | Dot <ocaml name="Point">
  | Arc <json name="arc"> <ocaml name="ArcShape"> of float
]
|};

  test "polymorphic variants"
    {|type status = [
  | Active
  | Inactive
  | Pending of string
] <ocaml repr="poly">
|};

  test "records"
    {|type person = {
  name: string;
  age: int;
  ?email: string option;
  ~score: float;
  ~active: bool;
  ~tags: string list;
  ~level <ocaml default="1">: int;
  address <json name="addr">: string;
}
|};

  test "builtin types"
    {|type all_types = {
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

  test "parametric types"
    {|type 'a result = [
  | Ok of 'a
  | Error of string
]

type ('a, 'b) either = [
  | Left of 'a
  | Right of 'b
]

type 'a page = {
  items: 'a list;
  total: int;
  ?cursor: string option;
}
|};

  test "mutually recursive types"
    {|type tree = [
  | Leaf
  | Node of node
]
type node = {
  value: int;
  children: tree list;
}
|};

]

let () =
  Testo.interpret_argv
    ~project_name:"atdml"
    tests
