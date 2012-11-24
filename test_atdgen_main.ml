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

let check_valid = function
  | None -> ()
  | Some error ->
      printf "%s\n%!" (Ag_util.Validation.string_of_error error);
      fail ()

let check_invalid = function
  | None -> fail ()
  | Some error ->
      printf "%s\n%!" (Ag_util.Validation.string_of_error error)


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

let test_extended_record = {
  Test.b0x = 55;
  b1x = false;
  b2x = "abc";
  b3x = Some "def";
  b4x = Some "ghi";
  b5x = 1.1;
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

let test_json_extra_field_warning () =
  section "json extra field warning";
  ignore (Testj.base_of_string (Testj.string_of_extended test_extended_record))

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
        field13 = [ Some "abc"; None ];
        field14 = (2012, Some 3, None);
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
      field13 = [ Some "abc"; None ];
      field14 = (2012, Some 3, None);
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

let test_json_space () =
  section "json space";
  let s = Testj.string_of_test test_correctness_data in
  let pp = Yojson.Safe.prettify s in
  ignore (Testj.test_of_string pp)


let test_validators0 () =
  section "validators0";
  check_valid (Testv.validate_test [] test_correctness_data)

let test_validators1 () =
  section "validators1";
  let valid = (0, 1.) in
  let invalid = (1, 0.) in
  check_valid (Testv.validate_base_tuple [] valid);
  check_invalid (Testv.validate_base_tuple [] invalid);

  let x1 = {
    Test.b0x = 1;
    b1x = true;
    b2x = "abc";
    b3x = Some "def";
    b4x = Some "ghi";
    b5x = 1.1;
  }
  in
  check_invalid (Testv.validate_extended [] x1);
  let x2 = { x1 with Test.b1x = false } in
  check_valid (Testv.validate_extended [] x2);
  let x3 = { x2 with Test.b0x = -1 } in
  check_invalid (Testv.validate_extended [] x3)

let test_validators2 () =
  section "validators2";
  let v1 = `A in
  check_invalid (Testv.validate_p [] v1);
  let v2 = `B { Test.a = 0; b = true; c = `C } in
  check_valid (Testv.validate_p [] v2)

let test_validators3 () =
  section "validators3";
  let o = Some 0 in
  check_invalid (Testv.validate_option_validation [] o)

let test_validators4 () =
  section "validators4";
  let x = { Test.val2_x = { Test.val1_x = 0 };
            val2_y = Some { Test.val1_x = 1 } } in
  check_invalid (Testv.validate_val2 [] x)

let test_json_files () =
  section "json files";
  let x = Some 123 in
  let s = Ag_util.Json.to_string Testj.write_intopt x in
  print_endline s;
  let x' = Ag_util.Json.from_string Testj.read_intopt s in
  check (x = x');
  Ag_util.Json.to_file Testj.write_intopt "test-json-files.json" x;
  let x'' = Ag_util.Json.from_file Testj.read_intopt "test-json-files.json" in
  check (x = x'')

let test_json_streams () =
  section "json streams";
  let l = [ Some 1; None; Some 2; Some 3 ] in
  let s = Ag_util.Json.list_to_string Testj.write_intopt l in
  print_endline s;
  let l' = Ag_util.Json.list_from_string Testj.read_intopt s in
  check (l = l');
  Ag_util.Json.list_to_file Testj.write_intopt "test-json-streams.json" l;
  let l'' =
    Ag_util.Json.list_from_file Testj.read_intopt "test-json-streams.json"
  in
  check (l = l'')

let test_raw_json () =
  section "raw json";
  let x = { Test3j.foo = 12345;
            bar = `List [ `Int 12; `String "abc" ];
            baz = `Bool false }
  in
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

let test_wrapping_ints () =
  section "ocaml wrapping - ints";
  let x = Test_lib.Natural.wrap 7 in
  let json = Testj.string_of_natural x in
  let x' = Testj.natural_of_string json in
  check (x = x');

  let biniou = Test.string_of_natural x in
  let x'' = Test.natural_of_string biniou in
  check (x = x'');

  try ignore (Testj.natural_of_string "-1"); check false
  with Failure _ -> ()

let test_double_wrapping () =
  section "ocaml wrapping - double wrapping";
  let x = Test_lib.Even_natural.wrap (Test_lib.Natural.wrap 10) in
  let json = Testj.string_of_even_natural x in
  let x' = Testj.even_natural_of_string json in
  check (x = x')

let test_wrapping_with_validation () =
  section "ocaml wrapping - with validation";
  let x = `Id "" in
  try ignore (Testv.validate_id [] x); check false
  with Failure "empty" -> ()

let test_ignored_wrap () =
  section "ocaml wrapping - wrap constructor without wrapper";
  let x = { Test.some_field = 0 } in
  try ignore (Testv.validate_no_real_wrap [] x); check false
  with Failure "passed" -> ()

let all_tests = [
  test_ocaml_internals;
  test_biniou_missing_field;
  test_biniou_missing_cell;
  test_json_missing_field;
  test_json_missing_cell;
  test_json_extra_field_warning;
  test_json_assoc_list;
  test_json_assoc_array;
  test_biniou_correctness;
  test_json_correctness;
  test_json_space;
  test_validators0;
  test_validators1;
  test_validators2;
  test_validators3;
  test_validators4;
  test_json_files;
  test_json_streams;
  test_raw_json;
  test_biniou_sharing_graph;
  test_biniou_sharing_strings;
  test_wrapping_ints;
  test_double_wrapping;
  test_wrapping_with_validation;
  test_ignored_wrap;
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
