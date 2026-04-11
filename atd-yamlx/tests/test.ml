(*
   Unit tests for atd-yamlx.

   Run from the atd-yamlx/ directory:
     dune runtest
*)

open Atd_jsonlike

(* ===== Helpers ===== *)

(* Build a YAMLx.loc spanning a single line. *)
let yaml_loc ?(file = "") line col_start col_end : YAMLx.loc =
  ignore file;
  let pos line col : YAMLx.pos = {
    line; column = col; column_bytes = col; offset = 0; offset_bytes = 0;
  } in
  { YAMLx.start_pos = pos line col_start; end_pos = pos line col_end }

(* Expected Atd_jsonlike.Loc.t for the same range (0-based row). *)
let expected_loc ?path row col_start col_end : Loc.t = {
  Loc.start = Pos.{ row; column = col_start };
  Loc.end_  = Pos.{ row; column = col_end   };
  Loc.path;
}

(* ===== Location translation ===== *)

(* YAMLx line numbers are 1-based; Atd_jsonlike rows are 0-based. *)
let test_loc_row_offset () =
  (* YAMLx line 3 (1-based) → Atd_jsonlike row 2 (0-based).
     AST.loc_msg displays rows in 1-based notation, so row 2 → "line 3". *)
  let v = YAMLx.Bool (yaml_loc 3 0 4, true) in
  let node = Atd_yamlx.of_yamlx_value v in
  let msg = AST.loc_msg node in
  (* loc_msg for a single-line range at row 2 starts with "line 3" *)
  Testo.(check bool) true (String.length msg >= 6 && String.sub msg 0 6 = "line 3")

let test_loc_no_path () =
  let v = YAMLx.Null (yaml_loc 1 0 0) in
  let node = Atd_yamlx.of_yamlx_value v in
  match node with
  | AST.Null loc ->
      Testo.(check (option string)) None loc.Loc.path;
      Testo.(check int) 0 loc.Loc.start.Pos.row  (* line 1 → row 0 *)
  | _ -> failwith "expected Null"

let test_loc_with_path () =
  let v = YAMLx.String (yaml_loc 5 2 7, "hello") in
  let node = Atd_yamlx.of_yamlx_value ~path:"config.yaml" v in
  match node with
  | AST.String (loc, s) ->
      Testo.(check string) "hello" s;
      Testo.(check (option string)) (Some "config.yaml") loc.Loc.path;
      Testo.(check int) 4 loc.Loc.start.Pos.row;   (* line 5 → row 4 *)
      Testo.(check int) 2 loc.Loc.start.Pos.column
  | _ -> failwith "expected String"

(* ===== Scalar value translation ===== *)

let test_null () =
  let v = YAMLx.Null (yaml_loc 1 0 4) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Null _ -> ()
  | _ -> failwith "expected Null"

let test_bool_true () =
  let v = YAMLx.Bool (yaml_loc 1 0 4, true) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Bool (_, b) -> Testo.(check bool) true b
  | _ -> failwith "expected Bool"

let test_bool_false () =
  let v = YAMLx.Bool (yaml_loc 1 0 5, false) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Bool (_, b) -> Testo.(check bool) false b
  | _ -> failwith "expected Bool"

let test_int_small () =
  let v = YAMLx.Int (yaml_loc 1 0 2, 42L) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Number (_, n) ->
      Testo.(check (option int)) (Some 42) n.Number.int;
      Testo.(check (option float)) (Some 42.) n.Number.float
  | _ -> failwith "expected Number"

let test_int_negative () =
  let v = YAMLx.Int (yaml_loc 1 0 2, (-7L)) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Number (_, n) ->
      Testo.(check (option int)) (Some (-7)) n.Number.int
  | _ -> failwith "expected Number"

(* int64 value too large for native int on 32-bit platforms but representable
   as a decimal literal. On 64-bit platforms this also fits in native int. *)
let test_int_large () =
  let v = YAMLx.Int (yaml_loc 1 0 10, 2147483648L) in  (* 2^31 *)
  match Atd_yamlx.of_yamlx_value v with
  | AST.Number (_, n) ->
      (* Must have at least the literal representation *)
      Testo.(check bool) true
        (n.Number.int <> None || n.Number.literal <> None)
  | _ -> failwith "expected Number"

let test_float () =
  let v = YAMLx.Float (yaml_loc 1 0 3, 3.14) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Number (_, n) ->
      Testo.(check (option float)) (Some 3.14) n.Number.float
  | _ -> failwith "expected Number"

let test_float_whole () =
  (* A float with an integer value should populate the int field *)
  let v = YAMLx.Float (yaml_loc 1 0 3, 2.0) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Number (_, n) ->
      Testo.(check (option int)) (Some 2) n.Number.int;
      Testo.(check (option float)) (Some 2.0) n.Number.float
  | _ -> failwith "expected Number"

let test_string () =
  let v = YAMLx.String (yaml_loc 1 0 5, "hello") in
  match Atd_yamlx.of_yamlx_value v with
  | AST.String (_, s) -> Testo.(check string) "hello" s
  | _ -> failwith "expected String"

(* ===== Sequence (Array) translation ===== *)

let test_seq_empty () =
  let v = YAMLx.Seq (yaml_loc 1 0 2, []) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Array (_, []) -> ()
  | _ -> failwith "expected empty Array"

let test_seq_of_ints () =
  let loc = yaml_loc 1 0 1 in
  let v = YAMLx.Seq (loc, [
    YAMLx.Int (yaml_loc 2 2 3, 1L);
    YAMLx.Int (yaml_loc 3 2 3, 2L);
    YAMLx.Int (yaml_loc 4 2 3, 3L);
  ]) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Array (_, items) ->
      Testo.(check int) 3 (List.length items);
      List.iteri (fun i node ->
        match node with
        | AST.Number (_, n) ->
            Testo.(check (option int)) (Some (i + 1)) n.Number.int
        | _ -> failwith "expected Number"
      ) items
  | _ -> failwith "expected Array"

(* ===== Map (Object) translation ===== *)

let test_map_empty () =
  let v = YAMLx.Map (yaml_loc 1 0 2, []) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Object (_, []) -> ()
  | _ -> failwith "expected empty Object"

let test_map_string_keys () =
  let loc = yaml_loc 1 0 1 in
  let v = YAMLx.Map (loc, [
    (yaml_loc 2 0 3, YAMLx.String (yaml_loc 2 0 3, "x"), YAMLx.Int (yaml_loc 2 5 6, 1L));
    (yaml_loc 3 0 3, YAMLx.String (yaml_loc 3 0 3, "y"), YAMLx.Int (yaml_loc 3 5 6, 2L));
  ]) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Object (_, [(_, "x", AST.Number (_, n1)); (_, "y", AST.Number (_, n2))]) ->
      Testo.(check (option int)) (Some 1) n1.Number.int;
      Testo.(check (option int)) (Some 2) n2.Number.int
  | _ -> failwith "expected Object with x/y keys"

(* Integer keys are converted to their decimal string representation *)
let test_map_int_key () =
  let loc = yaml_loc 1 0 1 in
  let v = YAMLx.Map (loc, [
    (yaml_loc 2 0 1, YAMLx.Int (yaml_loc 2 0 1, 42L), YAMLx.String (yaml_loc 2 3 8, "val"));
  ]) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Object (_, [(_, key, _)]) ->
      Testo.(check string) "42" key
  | _ -> failwith "expected single-entry Object"

(* Boolean keys are converted to "true" / "false" *)
let test_map_bool_key () =
  let loc = yaml_loc 1 0 1 in
  let v = YAMLx.Map (loc, [
    (yaml_loc 2 0 4, YAMLx.Bool (yaml_loc 2 0 4, true), YAMLx.Null (yaml_loc 2 6 10));
  ]) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Object (_, [(_, key, _)]) ->
      Testo.(check string) "true" key
  | _ -> failwith "expected single-entry Object"

(* Sequence keys raise Invalid_argument *)
let test_map_seq_key_raises () =
  let loc = yaml_loc 1 0 1 in
  let v = YAMLx.Map (loc, [
    (yaml_loc 2 0 1,
     YAMLx.Seq (yaml_loc 2 0 1, []),
     YAMLx.Null (yaml_loc 2 3 7));
  ]) in
  let raised =
    try ignore (Atd_yamlx.of_yamlx_value v); false
    with Invalid_argument _ -> true
  in
  Testo.(check bool) true raised

(* Nested map: { outer: { inner: 42 } } *)
let test_nested_map () =
  let v = YAMLx.Map (yaml_loc 1 0 1, [
    (yaml_loc 1 0 5,
     YAMLx.String (yaml_loc 1 0 5, "outer"),
     YAMLx.Map (yaml_loc 2 2 3, [
       (yaml_loc 2 2 7,
        YAMLx.String (yaml_loc 2 2 7, "inner"),
        YAMLx.Int   (yaml_loc 2 9 11, 42L));
     ]));
  ]) in
  match Atd_yamlx.of_yamlx_value v with
  | AST.Object (_, [(_, "outer", AST.Object (_, [(_, "inner", AST.Number (_, n))]))]) ->
      Testo.(check (option int)) (Some 42) n.Number.int
  | _ -> failwith "expected nested Object"

(* ===== Test list ===== *)

let tests _env = [
  Testo.create "loc: row is 0-based (line is 1-based in YAMLx)" test_loc_row_offset;
  Testo.create "loc: no path by default"                         test_loc_no_path;
  Testo.create "loc: path propagated to all nodes"               test_loc_with_path;
  Testo.create "scalar: Null"                                    test_null;
  Testo.create "scalar: Bool true"                               test_bool_true;
  Testo.create "scalar: Bool false"                              test_bool_false;
  Testo.create "scalar: Int small"                               test_int_small;
  Testo.create "scalar: Int negative"                            test_int_negative;
  Testo.create "scalar: Int large (2^31)"                        test_int_large;
  Testo.create "scalar: Float fractional"                        test_float;
  Testo.create "scalar: Float whole number"                      test_float_whole;
  Testo.create "scalar: String"                                  test_string;
  Testo.create "seq: empty"                                      test_seq_empty;
  Testo.create "seq: list of ints"                               test_seq_of_ints;
  Testo.create "map: empty"                                      test_map_empty;
  Testo.create "map: string keys"                                test_map_string_keys;
  Testo.create "map: int key → decimal string"                   test_map_int_key;
  Testo.create "map: bool key → \"true\"/\"false\""             test_map_bool_key;
  Testo.create "map: seq key raises Invalid_argument"            test_map_seq_key_raises;
  Testo.create "map: nested maps"                                test_nested_map;
]

let () =
  Testo.interpret_argv
    ~project_name:"atd-yamlx"
    tests
