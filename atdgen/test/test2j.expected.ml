(* Auto-generated from "test2.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]
open Test
open Test2
open Testj

let write_poly write__aa write__bb = (
  Testj.write_poly write__aa write__bb
)
let string_of_poly write__aa write__bb ?(len = 1024) x =
  let ob = Buffer.create len in
  write_poly write__aa write__bb ob x;
  Buffer.contents ob
let read_poly read__aa read__bb = (
  Testj.read_poly read__aa read__bb
)
let poly_of_string read__aa read__bb s =
  read_poly read__aa read__bb (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_int_poly = (
  write_poly Yojson.Safe.write_int Yojson.Safe.write_int
)
let string_of__int_int_poly ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_int_poly ob x;
  Buffer.contents ob
let read__int_int_poly = (
  read_poly Atdgen_runtime.Oj_run.read_int Atdgen_runtime.Oj_run.read_int
)
let _int_int_poly_of_string s =
  read__int_int_poly (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_poly_int2 = (
  write__int_int_poly
)
let string_of_poly_int2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_poly_int2 ob x;
  Buffer.contents ob
let read_poly_int2 = (
  read__int_int_poly
)
let poly_int2_of_string s =
  read_poly_int2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_option = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_string
  )
)
let string_of__string_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_option ob x;
  Buffer.contents ob
let read__string_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _string_option_of_string s =
  read__string_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_string_option_poly = (
  write_poly Yojson.Safe.write_int write__string_option
)
let string_of__int_string_option_poly ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_string_option_poly ob x;
  Buffer.contents ob
let read__int_string_option_poly = (
  read_poly Atdgen_runtime.Oj_run.read_int read__string_option
)
let _int_string_option_poly_of_string s =
  read__int_string_option_poly (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_test2 : _ -> test2 -> _ = (
  fun ob (x : test2) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"test0\":";
    (
      write_poly_int2
    )
      ob x.test0;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"test1\":";
    (
      write__int_string_option_poly
    )
      ob x.test1;
    Buffer.add_char ob '}';
)
let string_of_test2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_test2 ob x;
  Buffer.contents ob
let read_test2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_test0 = ref (None) in
    let field_test1 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' then (
            match String.unsafe_get s (pos+4) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_test0 := (
              Some (
                (
                  read_poly_int2
                ) p lb
              )
            );
          | 1 ->
            field_test1 := (
              Some (
                (
                  read__int_string_option_poly
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' then (
              match String.unsafe_get s (pos+4) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_test0 := (
                Some (
                  (
                    read_poly_int2
                  ) p lb
                )
              );
            | 1 ->
              field_test1 := (
                Some (
                  (
                    read__int_string_option_poly
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            test0 = (match !field_test0 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "test0");
            test1 = (match !field_test1 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "test1");
          }
         : test2)
      )
)
let test2_of_string s =
  read_test2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_string_poly = (
  write_poly Yojson.Safe.write_int Yojson.Safe.write_string
)
let string_of__int_string_poly ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_string_poly ob x;
  Buffer.contents ob
let read__int_string_poly = (
  read_poly Atdgen_runtime.Oj_run.read_int Atdgen_runtime.Oj_run.read_string
)
let _int_string_poly_of_string s =
  read__int_string_poly (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_poly_int_string = (
  write__int_string_poly
)
let string_of_poly_int_string ?(len = 1024) x =
  let ob = Buffer.create len in
  write_poly_int_string ob x;
  Buffer.contents ob
let read_poly_int_string = (
  read__int_string_poly
)
let poly_int_string_of_string s =
  read_poly_int_string (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let create_test2 
  ~test0
  ~test1
  () : test2 =
  {
    test0 = test0;
    test1 = test1;
  }
