(*
   Unit tests for atd-jsonlike.

   Run from the atd-jsonlike/ directory:
     ./test            -- run all tests
     ./test status     -- show results without re-running
*)

open Atd_jsonlike

(* Convenience: assert equality with a descriptive message on failure. *)
let check_eq label pp_val actual expected =
  if actual <> expected then
    failwith (Printf.sprintf "%s: expected %s, got %s"
                label (pp_val expected) (pp_val actual))

let pp_opt pp = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (pp x)

let pp_int = string_of_int
let pp_float = string_of_float

(* ===== Number.of_int ===== *)

let test_of_int () =
  let n = Number.of_int 42 in
  check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some 42);
  check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 42.);
  check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "42")

let test_of_int_neg () =
  let n = Number.of_int (-7) in
  check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some (-7));
  check_eq "float"   (pp_opt pp_float) n.Number.float   (Some (-7.));
  check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "-7")

let test_of_int_zero () =
  let n = Number.of_int 0 in
  check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some 0);
  check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 0.);
  check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "0")

(* ===== Number.of_float ===== *)

(* 1.5 is not an integer — int should be None, literal not preserved. *)
let test_of_float_frac () =
  let n = Number.of_float 1.5 in
  check_eq "int"     (pp_opt pp_int)   n.Number.int     None;
  check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 1.5);
  check_eq "literal" (pp_opt Fun.id)   n.Number.literal None

(* 3.0 has an integer value — int should be populated. *)
let test_of_float_whole () =
  let n = Number.of_float 3.0 in
  check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some 3);
  check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 3.);
  check_eq "literal" (pp_opt Fun.id)   n.Number.literal None

(* ===== Number.of_string_opt — valid inputs ===== *)

(* Plain integer *)
let test_of_string_int () =
  match Number.of_string_opt "42" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some 42);
    check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 42.);
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "42")

(* Negative integer *)
let test_of_string_neg_int () =
  match Number.of_string_opt "-3" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some (-3));
    check_eq "float"   (pp_opt pp_float) n.Number.float   (Some (-3.));
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "-3")

(* Decimal fraction — no integer representation *)
let test_of_string_frac () =
  match Number.of_string_opt "1.5" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     None;
    check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 1.5);
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "1.5")

(* Scientific notation: 1.2e3 = 1200.0 as a float.  int is None because
   int_of_string_opt does not parse scientific notation. *)
let test_of_string_sci_int () =
  match Number.of_string_opt "1.2e3" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     None;
    check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 1200.);
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "1.2e3")

(* 1e400 overflows both int and float, but the literal is preserved *)
let test_of_string_overflow () =
  match Number.of_string_opt "1e400" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     None;
    check_eq "float"   (pp_opt pp_float) n.Number.float   None;
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "1e400")

(* "0" is a valid JSON number *)
let test_of_string_zero () =
  match Number.of_string_opt "0" with
  | None -> failwith "expected Some"
  | Some n ->
    check_eq "int"     (pp_opt pp_int)   n.Number.int     (Some 0);
    check_eq "float"   (pp_opt pp_float) n.Number.float   (Some 0.);
    check_eq "literal" (pp_opt Fun.id)   n.Number.literal (Some "0")

(* ===== Number.of_string_opt — invalid inputs ===== *)

let test_of_string_invalid () =
  let invalid = [
    "+42";    (* '+' prefix not allowed in JSON *)
    "01";     (* leading zero *)
    "";       (* empty string *)
    "nan";    (* not a JSON token *)
    "inf";    (* not a JSON token *)
    ".5";     (* must start with digit or '-' *)
    "1.";     (* decimal point without trailing digits *)
    "--1";    (* double minus *)
    "1 ";     (* trailing space *)
    " 1";     (* leading space *)
    "1_000";  (* underscores not in JSON *)
  ] in
  List.iter (fun s ->
    match Number.of_string_opt s with
    | Some _ -> failwith (Printf.sprintf "expected None for %S" s)
    | None -> ()
  ) invalid

(* ===== AST.loc_msg ===== *)

let dummy_loc start_row start_col end_row end_col =
  Loc.{
    start = Pos.{ row = start_row; column = start_col };
    end_  = Pos.{ row = end_row;   column = end_col   };
    path  = None;
  }

(* Single-line location without a file path *)
let test_loc_msg_single_line () =
  let loc = dummy_loc 2 0 2 10 in
  let node = AST.String (loc, "hello") in
  let msg = AST.loc_msg node in
  (* row is 0-based internally, displayed as 1-based *)
  check_eq "loc_msg" Fun.id msg "line 3, characters 0-10:\n"

(* Multi-line location *)
let test_loc_msg_multi_line () =
  let loc = dummy_loc 1 5 3 2 in
  let node = AST.Null loc in
  let msg = AST.loc_msg node in
  check_eq "loc_msg" Fun.id msg "lines 2-4, characters 5-2:\n"

(* Location with a file path *)
let test_loc_msg_with_path () =
  let loc = { (dummy_loc 0 0 0 5) with Loc.path = Some "config.yaml" } in
  let node = AST.Bool (loc, true) in
  let msg = AST.loc_msg node in
  check_eq "loc_msg" Fun.id msg "File \"config.yaml\", line 1, characters 0-5:\n"

(* ===== Test list ===== *)

let tests _env = [
  Testo.create "Number.of_int: 42"           test_of_int;
  Testo.create "Number.of_int: negative"      test_of_int_neg;
  Testo.create "Number.of_int: zero"          test_of_int_zero;
  Testo.create "Number.of_float: fraction"    test_of_float_frac;
  Testo.create "Number.of_float: whole"       test_of_float_whole;
  Testo.create "Number.of_string_opt: int"    test_of_string_int;
  Testo.create "Number.of_string_opt: negative int" test_of_string_neg_int;
  Testo.create "Number.of_string_opt: fraction" test_of_string_frac;
  Testo.create "Number.of_string_opt: scientific notation integer" test_of_string_sci_int;
  Testo.create "Number.of_string_opt: overflow" test_of_string_overflow;
  Testo.create "Number.of_string_opt: zero"   test_of_string_zero;
  Testo.create "Number.of_string_opt: invalid inputs" test_of_string_invalid;
  Testo.create "AST.loc_msg: single line"     test_loc_msg_single_line;
  Testo.create "AST.loc_msg: multi-line"      test_loc_msg_multi_line;
  Testo.create "AST.loc_msg: with file path"  test_loc_msg_with_path;
]

let () =
  Testo.interpret_argv
    ~project_name:"atd-jsonlike"
    tests
