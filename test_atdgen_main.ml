(* $Id: test_atdgen_main.ml 53195 2011-01-28 23:38:12Z martin $ *)

open Printf

let current_section = ref ""

let section =
  let first_section = ref true in
  fun name ->
    current_section := name;
    if !first_section then
      first_section := false;
    printf "----- %s -----\n%!" name

let errors = ref []

exception Failed

let fail () =
  errors := !current_section :: !errors;
  raise Failed

let check b =
  if not b then fail ()

let expect_error f x =
  try 
    ignore (f x);
    printf "Did not get expected error\n%!";
    fail ()
  with
      Ag_ob_run.Error s 
    | Ag_oj_run.Error s ->
        printf "Got expected error:\n%s\n%!" s

let test_missing_record = {
  Test.b0 = 123;
  b1 = true
}

let test_missing_tuple = (123, 4.56)


type internals1 = { int1 : bool }
type internals2 = { mutable int2 : bool }

let test_ocaml_internals () =
  section "ocaml internals";
  let f () = { int1 = Obj.magic false } in
  check (f () != f ());
  let g () = { int1 = false } in
  check (g () == g ());
  let h () = { int2 = false } in
  check (h () != h ())

let test_biniou_missing_field () =
  section "biniou missing record fields";
  expect_error
    Test.extended_of_string (Test.string_of_base test_missing_record)

let test_biniou_missing_cell () =
  section "biniou missing tuple fields";
  expect_error 
    Test.extended_tuple_of_string
    (Test.string_of_base_tuple test_missing_tuple)
  
let test_json_missing_field () =
  section "json missing record fields";
  expect_error
    Testj.extended_of_string (Testj.string_of_base test_missing_record)

let test_json_missing_cell () =
  section "json missing tuple fields";
  expect_error 
    Testj.extended_tuple_of_string
    (Testj.string_of_base_tuple test_missing_tuple)

let test_json_assoc_list () =
  section "json association list";
  let f l =
    let s = Testj.string_of_int_assoc_list l in
    print_endline s;
    check (Testj.int_assoc_list_of_string s = l)
  in
  f [];
  f [ ("a", 0) ];
  f [ ("a", 0); ("b", 1) ]

let test_json_assoc_array () =
  section "json association array";
  let f a =
    let s = Testj.string_of_int_assoc_array a in
    print_endline s;
    check (Testj.int_assoc_array_of_string s = a)
  in
  f [| |];
  f [| ("a", 0) |];
  f [| ("a", 0); ("b", 1) |]


let make_mixed_record_array n =
  Array.init n (
    fun i -> 
      {
	Test.field0 = Some i;
	field1 = Some 0.555;
	field2 = Some (String.copy "abcdefghijklmnopqrstuvwxyz");
	field3 = 12345678L;
	field4 = [| 1.23; 3.45; 4.56 |];
	field5 = None;
	field6 = None;
	field7 = `Case4 [ `Case1; `Case2 999; `Case3 "abcdefghij"; `Case4 [] ];
	field8 = [| "a"; "bc"; "def"; "ghij"; "klmno";
		    "pqrstu"; "vwxyz01"; "23456789" |];
	field9 = (
	  1_000_000,
	  0xff,
	  '\xff',
	  0xffff,
	  0xffffffffl,
	  0xffffffffffffffffL
	);
	field10 = true;
	field11 = false;
        field12 = [ (); () ];
        field13 = [ Some "abc"; None ]
      }
  )

let make_mixed ~top_len ~tab_len ~ar_len =
  Array.to_list (
    Array.init top_len (
      fun _ ->
	(make_mixed_record_array tab_len, make_mixed_record_array ar_len)
    )
  )


let test_correctness_data = {
  Test.x0 = Some 123;
  x1 = Some 1.23;
  x2 = make_mixed ~top_len:2 ~tab_len:2 ~ar_len:2;
  x3 = [
    {
      Test.field0 = Some 1234;
      field1 = Some 1e6;
      field2 = Some "Hello";
      field3 = 12345678L;
      field4 = [| 1.23; 3.45; 5.67 |];
      field5 = None;
      field6 = None;
      field7 = `Case4 [ `Case1; `Case2 999; `Case3 "abcdefghij"; `Case4 [] ];
      field8 = [| "abcdef"; "0123456789" |];
      field9 = (
	1_000_000,
	0xff,
	'\xff',
	0xffff,
	0xffffffffl,
	0xffffffffffffffffL
      );
      field10 = true;
      field11 = false;
      field12 = [ (); () ];
      field13 = [ Some "abc"; None ]
    }
  ];
  x4 = 0x0807060504030201L;
}

let save file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let test_biniou_correctness () =
  section "biniou correctness";
  let x = test_correctness_data in
  let s = Test.string_of_test x in
  save "test.bin" s;
  let x' = Test.test_of_string s in
  let s' = Test.string_of_test x' in
  let x'' = Test.test_of_string s' in
  save "test-2.bin" s';
  if x <> x' then (
    print_endline (Bi_io.view s);
    print_endline "Data don't match";
    if s = s' then
      print_endline "Strings match"
    else
      print_endline "Strings don't match either";
    if x' = x'' then
      print_endline "2nd and 3rd generation data match"
    else
      print_endline "2nd and 3rd generation data differ";
    fail ()
  )

let test_json_correctness () =
  section "json correctness";
  let x = test_correctness_data in
  let s = Testj.string_of_test x in
  save "test.json" s;
  let x' = Testj.test_of_string s in
  let s' = Testj.string_of_test x' in
  let x'' = Testj.test_of_string s' in
  save "test-2.json" s';
  let std_x' = Testjstd.test_of_string s in
  let std_s' = Testjstd.string_of_test std_x' in
  let std_x'' = Testjstd.test_of_string std_s' in
  save "test-std.json" std_s';
  if x <> x' then (
    print_endline (Yojson.Safe.prettify s);
    print_endline "Data don't match";
    if s = s' then
      print_endline "Strings match"
    else
      print_endline "Strings don't match either";
    if x' = x'' then
      print_endline "2nd and 3rd generation data match"
    else
      print_endline "2nd and 3rd generation data differ";
    fail ()
  );
  check (std_x' = std_x'');
  assert (x = std_x')

let test_raw_json () =
  section "raw json";
  let x = { Test3j.foo = 12345; bar = `List [ `Int 12; `String "abc" ] } in
  let s = Test3j.string_of_t x in
  let x' = Test3j.t_of_string s in
  check (x = x')
  
let test_biniou_sharing_graph () =
  section "biniou sharing - graph";
  let a = { Test3b.value = "a"; neighbors = [] } in
  let b = { Test3b.value = "b"; neighbors = [] } in
  a.Test3b.neighbors <- [ a; b ];
  b.Test3b.neighbors <- [ b; a ];
  let g = [ a; b ] in
  let s = Test3b.string_of_graph g in
  let g' = Test3b.graph_of_string s in
  check (g != g');
  let a', b' =
    match g' with
        [ a'; b' ] -> a', b'
      | _ -> fail ()
  in
  let a'', b'' =
    match a'.Test3b.neighbors with
        [ a''; b''  ] -> a'', b''
      | _ -> fail ()
  in
  check (a' == a'');
  check (b' == b'')
  
let test_biniou_sharing_strings () =
  section "biniou sharing - strings";
  let x = ref "abc" in
  let a = [| x; x |] in
  let b = x in
  let a' = Test3b.a_of_string (Test3b.string_of_a a) in
  let b' = Test3b.b_of_string (Test3b.string_of_b b) in
  check (a.(0) == b);
  check (a.(0) == a.(1));
  check (a'.(0) != a.(0));
  check (a'.(0) == a'.(1));
  check (a'.(0) != b')

let all_tests = [
  test_ocaml_internals;
  test_biniou_missing_field;
  test_biniou_missing_cell;
  test_json_missing_field;
  test_json_missing_cell;
  test_json_assoc_list;
  test_json_assoc_array;
  test_biniou_correctness;
  test_json_correctness;
  test_raw_json;
  test_biniou_sharing_graph;
  test_biniou_sharing_strings;
]

let quality_test () =
  List.iter (fun f ->
               try f (); print_endline "Passed."
               with Failed -> ())
    all_tests;
  match List.rev !errors with
      [] -> printf "\nSUCCESS\n"
    | l ->
	printf "\nThe following tests failed:\n%s\n"
	  (String.concat "\n" l);
	printf "*** FAILURE ***\n"

let () = quality_test ()
