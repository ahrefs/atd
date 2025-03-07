(* Auto-generated from "test.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

(** This is just a test. *)

type test_variant = Test.test_variant

type ('x, 'y) poly = ('x, 'y) Test.poly = {
  fst: 'x list;
  snd: ('x, 'y) poly option
}

type 'a p' = 'a Test.p' =  A | Bb of 'a p' | Ccccc of 'a 

type p = Test.p

and r = Test.r = { a: int; mutable b: bool; c: p }

type validated_string_check = Test.validated_string_check

type validate_me = Test.validate_me

type val1 = Test.val1 = { val1_x: int }

type val2 = Test.val2 = { val2_x: val1; val2_y: val1 option }

type unixtime_list = Test.unixtime_list

type date = Test.date

type mixed_record = Test.mixed_record = {
  field0: int option;
  field1: float option;
  field2: string option;
  field3: Int64.t;
  field4: float Atdgen_runtime.Util.ocaml_array;
  field5: bool option;
  field6: string option;
  field7: test_variant;
  field8: string Atdgen_runtime.Util.ocaml_array;
  field9: (int * int * Char.t * int * Int32.t * Int64.t);
  field10: bool;
  field11: bool;
  field12: unit list;
  field13: string option list;
  field14: date
}

type mixed = Test.mixed

type test = Test.test = {
  x0: int option;
  x1: float option;
  x2: mixed;
  x3: mixed_record list;
  x4: Int64.t
}

type tup = Test.tup

type test_field_prefix = Test.test_field_prefix = {
  theprefix_hello (*atd hello *): bool;
  theprefix_world (*atd world *): int
}

type star_rating = Test.star_rating

type 'a generic = 'a Test.generic = { x294623: int }

type specialized = Test.specialized

type some_record = Test.some_record = { some_field: int }

type precision = Test.precision = {
  sqrt2_5: float;
  small_2: float;
  large_2: float
}

type p'' = Test.p''

type option_validation = Test.option_validation

type no_real_wrap = Test.no_real_wrap

type natural = Test.natural

type id = Test.id

type json_map = Test.json_map

type intopt = Test.intopt

type int_assoc_list = Test.int_assoc_list

type int_assoc_array = Test.int_assoc_array

type int8 = Test.int8

type int64 = Test.int64

type int32 = Test.int32

type hello = Test.hello

type floats = Test.floats = { f32: float; f64: float }

type extended_tuple = Test.extended_tuple

type extended = Test.extended = {
  b0x (*atd b0 *): int;
  b1x (*atd b1 *): bool;
  b2x (*atd b2 *): string;
  b3x (*atd b3 *): string option;
  b4x (*atd b4 *): string option;
  b5x (*atd b5 *): float
}

type even_natural = Test.even_natural

(**
  \}\}\}abc[def]ghi
  
{v
j  *  j
 k * k
  l*l
v}
  
{v
mno
v}
  
  [pqr]\{stu\}vwx
  
  yz
  
  [\} \[ \] \{v]
  
{v
\} [x] v\} \{v [ @ 
v}
*)
type def = Test_lib.Json.def

type char = Test.char

type base_tuple = Test.base_tuple

type base = Test.base = { b0: int; b1: bool }

type 'a array = 'a Test.array

type 'a abs3 = 'a Test.abs3

type 'a abs2 = 'a Test.abs2

type 'a abs1 = 'a Test.abs1

let write__a_list write__a = (
  Atdgen_runtime.Oj_run.write_list (
    write__a
  )
)
let string_of__a_list write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write__a_list write__a ob x;
  Buffer.contents ob
let read__a_list read__a = (
  Atdgen_runtime.Oj_run.read_list (
    read__a
  )
)
let _a_list_of_string read__a s =
  read__a_list read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write_p' write__a : _ -> 'a p' -> _ = (
  fun ob (x : 'a p') ->
    match x with
      | A -> Buffer.add_string ob "\"A\""
      | Bb x ->
        Buffer.add_string ob "[\"Bb\",";
        (
          write_p' write__a
        ) ob x;
        Buffer.add_char ob ']'
      | Ccccc x ->
        Buffer.add_string ob "[\"Ccccc\",";
        (
          write__a
        ) ob x;
        Buffer.add_char ob ']'
)
and string_of_p' write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_p' write__a ob x;
  Buffer.contents ob
let rec read_p' read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A : 'a p')
            | "Bb" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_p' read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Bb x : 'a p')
            | "Ccccc" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Ccccc x : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              (A : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Bb" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_p' read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Bb x : 'a p')
            | "Ccccc" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Ccccc x : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and p'_of_string read__a s =
  read_p' read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write_p = (
  fun ob x ->
    match x with
      | `A -> Buffer.add_string ob "\"A\""
      | `B x ->
        Buffer.add_string ob "[\"B\",";
        (
          write_r
        ) ob x;
        Buffer.add_char ob ']'
      | `C -> Buffer.add_string ob "\"C\""
)
and string_of_p ?(len = 1024) x =
  let ob = Buffer.create len in
  write_p ob x;
  Buffer.contents ob
and write_r : _ -> r -> _ = (
  fun ob (x : r) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"a\":";
    (
      Yojson.Safe.write_int
    )
      ob x.a;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"c\":";
    (
      write_p
    )
      ob x.c;
    Buffer.add_char ob '}';
)
and string_of_r ?(len = 1024) x =
  let ob = Buffer.create len in
  write_r ob x;
  Buffer.contents ob
let rec read_p = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `A
            | "B" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_r
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `B x
            | "C" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `C
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              `A
            | "C" ->
              `C
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "B" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_r
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `B x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and p_of_string s =
  read_p (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_r = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_a = ref (None) in
    let field_b = ref (None) in
    let field_c = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 1 then (
            match String.unsafe_get s pos with
              | 'a' -> (
                  0
                )
              | 'b' -> (
                  1
                )
              | 'c' -> (
                  2
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
            field_a := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 2 ->
            field_c := (
              Some (
                (
                  read_p
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
            if len = 1 then (
              match String.unsafe_get s pos with
                | 'a' -> (
                    0
                  )
                | 'b' -> (
                    1
                  )
                | 'c' -> (
                    2
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
              field_a := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 2 ->
              field_c := (
                Some (
                  (
                    read_p
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
            a = (match !field_a with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "a");
            b = (match !field_b with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b");
            c = (match !field_c with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "c");
          }
         : r)
      )
)
and r_of_string s =
  read_r (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__test_variant_list ob x = (
  Atdgen_runtime.Oj_run.write_list (
    write_test_variant
  )
) ob x
and string_of__test_variant_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__test_variant_list ob x;
  Buffer.contents ob
and write_test_variant = (
  fun ob x ->
    match x with
      | `Case1 -> Buffer.add_string ob "\"Case1\""
      | `Case2 x ->
        Buffer.add_string ob "[\"Case2\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Buffer.add_char ob ']'
      | `Case3 x ->
        Buffer.add_string ob "[\"Case3\",";
        (
          Yojson.Safe.write_string
        ) ob x;
        Buffer.add_char ob ']'
      | `Case4 x ->
        Buffer.add_string ob "[\"Case4\",";
        (
          write__test_variant_list
        ) ob x;
        Buffer.add_char ob ']'
)
and string_of_test_variant ?(len = 1024) x =
  let ob = Buffer.create len in
  write_test_variant ob x;
  Buffer.contents ob
let rec read__test_variant_list p lb = (
  Atdgen_runtime.Oj_run.read_list (
    read_test_variant
  )
) p lb
and _test_variant_list_of_string s =
  read__test_variant_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_test_variant = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Case1" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case1
            | "Case2" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case2 x
            | "Case3" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case3 x
            | "Case4" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__test_variant_list
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case4 x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Case1" ->
              `Case1
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Case2" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case2 x
            | "Case3" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case3 x
            | "Case4" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__test_variant_list
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case4 x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and test_variant_of_string s =
  read_test_variant (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__int_p : _ -> _ p' -> _ = (
  fun ob (x : _ p') ->
    match x with
      | A -> Buffer.add_string ob "\"A\""
      | Bb x ->
        Buffer.add_string ob "[\"Bb\",";
        (
          write__int_p
        ) ob x;
        Buffer.add_char ob ']'
      | Ccccc x ->
        Buffer.add_string ob "[\"Ccccc\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Buffer.add_char ob ']'
)
and string_of__int_p ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_p ob x;
  Buffer.contents ob
let rec read__int_p = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A : _ p')
            | "Bb" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__int_p
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Bb x : _ p')
            | "Ccccc" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Ccccc x : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              (A : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Bb" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__int_p
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Bb x : _ p')
            | "Ccccc" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Ccccc x : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and _int_p_of_string s =
  read__int_p (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__a_b_poly_option write__a write__b ob x = (
  Atdgen_runtime.Oj_run.write_std_option (
    write_poly write__a write__b
  )
) ob x
and string_of__a_b_poly_option write__a write__b ?(len = 1024) x =
  let ob = Buffer.create len in
  write__a_b_poly_option write__a write__b ob x;
  Buffer.contents ob
and write_poly write__x write__y : _ -> ('x, 'y) poly -> _ = (
  fun ob (x : ('x, 'y) poly) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"fst\":";
    (
      write__a_list write__x
    )
      ob x.fst;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"snd\":";
    (
      write__a_b_poly_option write__x write__y
    )
      ob x.snd;
    Buffer.add_char ob '}';
)
and string_of_poly write__x write__y ?(len = 1024) x =
  let ob = Buffer.create len in
  write_poly write__x write__y ob x;
  Buffer.contents ob
let rec read__a_b_poly_option read__a read__b = (
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
                  read_poly read__a read__b
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
                  read_poly read__a read__b
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and _a_b_poly_option_of_string read__a read__b s =
  read__a_b_poly_option read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_poly read__x read__y = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_fst = ref (None) in
    let field_snd = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 3 then (
            match String.unsafe_get s pos with
              | 'f' -> (
                  if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 's' -> (
                  if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
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
            field_fst := (
              Some (
                (
                  read__a_list read__x
                ) p lb
              )
            );
          | 1 ->
            field_snd := (
              Some (
                (
                  read__a_b_poly_option read__x read__y
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
            if len = 3 then (
              match String.unsafe_get s pos with
                | 'f' -> (
                    if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 't' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 's' -> (
                    if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                      1
                    )
                    else (
                      -1
                    )
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
              field_fst := (
                Some (
                  (
                    read__a_list read__x
                  ) p lb
                )
              );
            | 1 ->
              field_snd := (
                Some (
                  (
                    read__a_b_poly_option read__x read__y
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
            fst = (match !field_fst with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "fst");
            snd = (match !field_snd with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "snd");
          }
         : ('x, 'y) poly)
      )
)
and poly_of_string read__x read__y s =
  read_poly read__x read__y (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_validated_string_check = (
  Yojson.Safe.write_string
)
let string_of_validated_string_check ?(len = 1024) x =
  let ob = Buffer.create len in
  write_validated_string_check ob x;
  Buffer.contents ob
let read_validated_string_check = (
  Atdgen_runtime.Oj_run.read_string
)
let validated_string_check_of_string s =
  read_validated_string_check (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_5640b64 = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__x_5640b64 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_5640b64 ob x;
  Buffer.contents ob
let read__x_5640b64 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _x_5640b64_of_string s =
  read__x_5640b64 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_validate_me = (
  write__x_5640b64
)
let string_of_validate_me ?(len = 1024) x =
  let ob = Buffer.create len in
  write_validate_me ob x;
  Buffer.contents ob
let read_validate_me = (
  read__x_5640b64
)
let validate_me_of_string s =
  read_validate_me (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_val1 : _ -> val1 -> _ = (
  fun ob (x : val1) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"val1_x\":";
    (
      Yojson.Safe.write_int
    )
      ob x.val1_x;
    Buffer.add_char ob '}';
)
let string_of_val1 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_val1 ob x;
  Buffer.contents ob
let read_val1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_val1_x = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '1' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'x' then (
            0
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
            field_val1_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
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
            if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '1' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'x' then (
              0
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
              field_val1_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
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
            val1_x = (match !field_val1_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "val1_x");
          }
         : val1)
      )
)
let val1_of_string s =
  read_val1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__val1_option = (
  Atdgen_runtime.Oj_run.write_std_option (
    write_val1
  )
)
let string_of__val1_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__val1_option ob x;
  Buffer.contents ob
let read__val1_option = (
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
                  read_val1
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
                  read_val1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _val1_option_of_string s =
  read__val1_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_val2 : _ -> val2 -> _ = (
  fun ob (x : val2) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"val2_x\":";
    (
      write_val1
    )
      ob x.val2_x;
    (match x.val2_y with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"val2_y\":";
      (
        write_val1
      )
        ob x;
    );
    Buffer.add_char ob '}';
)
let string_of_val2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_val2 ob x;
  Buffer.contents ob
let read_val2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_val2_x = ref (None) in
    let field_val2_y = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '2' && String.unsafe_get s (pos+4) = '_' then (
            match String.unsafe_get s (pos+5) with
              | 'x' -> (
                  0
                )
              | 'y' -> (
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
            field_val2_x := (
              Some (
                (
                  read_val1
                ) p lb
              )
            );
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_val2_y := (
                Some (
                  (
                    read_val1
                  ) p lb
                )
              );
            )
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
            if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '2' && String.unsafe_get s (pos+4) = '_' then (
              match String.unsafe_get s (pos+5) with
                | 'x' -> (
                    0
                  )
                | 'y' -> (
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
              field_val2_x := (
                Some (
                  (
                    read_val1
                  ) p lb
                )
              );
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_val2_y := (
                  Some (
                    (
                      read_val1
                    ) p lb
                  )
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            val2_x = (match !field_val2_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "val2_x");
            val2_y = !field_val2_y;
          }
         : val2)
      )
)
let val2_of_string s =
  read_val2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_6089809 = (
  Atdgen_runtime.Oj_run.write_list (
    Atdgen_runtime.Oj_run.write_float_as_int
  )
)
let string_of__x_6089809 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_6089809 ob x;
  Buffer.contents ob
let read__x_6089809 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _x_6089809_of_string s =
  read__x_6089809 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_unixtime_list = (
  write__x_6089809
)
let string_of_unixtime_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write_unixtime_list ob x;
  Buffer.contents ob
let read_unixtime_list = (
  read__x_6089809
)
let unixtime_list_of_string s =
  read_unixtime_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_nullable = (
  Atdgen_runtime.Oj_run.write_nullable (
    Yojson.Safe.write_int
  )
)
let string_of__int_nullable ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_nullable ob x;
  Buffer.contents ob
let read__int_nullable = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      Atdgen_runtime.Oj_run.read_int
    ) p lb) : _ option)
)
let _int_nullable_of_string s =
  read__int_nullable (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_date = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x, _ = x in
    (
      write__int_nullable
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, x = x in
    (
      write__int_nullable
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_date ?(len = 1024) x =
  let ob = Buffer.create len in
  write_date ob x;
  Buffer.contents ob
let read_date = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read__int_nullable
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            read__int_nullable
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
)
let date_of_string s =
  read_date (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_adbef7e = (
  Atdgen_runtime.Oj_run.write_array (
    Yojson.Safe.write_std_float
  )
)
let string_of__x_adbef7e ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_adbef7e ob x;
  Buffer.contents ob
let read__x_adbef7e = (
  Atdgen_runtime.Oj_run.read_array (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _x_adbef7e_of_string s =
  read__x_adbef7e (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_20d39e2 = (
  Atdgen_runtime.Oj_run.write_array (
    Yojson.Safe.write_string
  )
)
let string_of__x_20d39e2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_20d39e2 ob x;
  Buffer.contents ob
let read__x_20d39e2 = (
  Atdgen_runtime.Oj_run.read_array (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _x_20d39e2_of_string s =
  read__x_20d39e2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__unit_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_null
  )
)
let string_of__unit_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__unit_list ob x;
  Buffer.contents ob
let read__unit_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_null
  )
)
let _unit_list_of_string s =
  read__unit_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write__string_option_list = (
  Atdgen_runtime.Oj_run.write_list (
    write__string_option
  )
)
let string_of__string_option_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_option_list ob x;
  Buffer.contents ob
let read__string_option_list = (
  Atdgen_runtime.Oj_run.read_list (
    read__string_option
  )
)
let _string_option_list_of_string s =
  read__string_option_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_option = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_int
  )
)
let string_of__int_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_option ob x;
  Buffer.contents ob
let read__int_option = (
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
                  Atdgen_runtime.Oj_run.read_int
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
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _int_option_of_string s =
  read__int_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__float_option = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_std_float
  )
)
let string_of__float_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__float_option ob x;
  Buffer.contents ob
let read__float_option = (
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
                  Atdgen_runtime.Oj_run.read_number
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
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _float_option_of_string s =
  read__float_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__bool_option = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_bool
  )
)
let string_of__bool_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__bool_option ob x;
  Buffer.contents ob
let read__bool_option = (
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
                  Atdgen_runtime.Oj_run.read_bool
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
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _bool_option_of_string s =
  read__bool_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_mixed_record : _ -> mixed_record -> _ = (
  fun ob (x : mixed_record) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    (match x.field0 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"field0\":";
      (
        Yojson.Safe.write_int
      )
        ob x;
    );
    (match x.field1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"field1\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field2\":";
    (
      write__string_option
    )
      ob x.field2;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field3\":";
    (
      Atdgen_runtime.Oj_run.write_int64
    )
      ob x.field3;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field4\":";
    (
      write__x_adbef7e
    )
      ob x.field4;
    (match x.field5 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"field5\":";
      (
        Yojson.Safe.write_bool
      )
        ob x;
    );
    (match x.field6 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"field6\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field7\":";
    (
      write_test_variant
    )
      ob x.field7;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field8\":";
    (
      write__x_20d39e2
    )
      ob x.field8;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field9\":";
    (
      fun ob x ->
        Buffer.add_char ob '[';
        (let x, _, _, _, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, x, _, _, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, _, x, _, _, _ = x in
        (
          Atdgen_runtime.Oj_run.write_int8
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, _, _, x, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, _, _, _, x, _ = x in
        (
          Atdgen_runtime.Oj_run.write_int32
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, _, _, _, _, x = x in
        (
          Atdgen_runtime.Oj_run.write_int64
        ) ob x
        );
        Buffer.add_char ob ']';
    )
      ob x.field9;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field10\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.field10;
    if x.field11 <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"field11\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.field11;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field12\":";
    (
      write__unit_list
    )
      ob x.field12;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field13\":";
    (
      write__string_option_list
    )
      ob x.field13;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"field14\":";
    (
      write_date
    )
      ob x.field14;
    Buffer.add_char ob '}';
)
let string_of_mixed_record ?(len = 1024) x =
  let ob = Buffer.create len in
  write_mixed_record ob x;
  Buffer.contents ob
let read_mixed_record = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_field0 = ref (None) in
    let field_field1 = ref (None) in
    let field_field2 = ref (None) in
    let field_field3 = ref (None) in
    let field_field4 = ref (None) in
    let field_field5 = ref (None) in
    let field_field6 = ref (None) in
    let field_field7 = ref (None) in
    let field_field8 = ref (None) in
    let field_field9 = ref (None) in
    let field_field10 = ref (None) in
    let field_field11 = ref (false) in
    let field_field12 = ref (None) in
    let field_field13 = ref (None) in
    let field_field14 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                  match String.unsafe_get s (pos+5) with
                    | '0' -> (
                        0
                      )
                    | '1' -> (
                        1
                      )
                    | '2' -> (
                        2
                      )
                    | '3' -> (
                        3
                      )
                    | '4' -> (
                        4
                      )
                    | '5' -> (
                        5
                      )
                    | '6' -> (
                        6
                      )
                    | '7' -> (
                        7
                      )
                    | '8' -> (
                        8
                      )
                    | '9' -> (
                        9
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '1' then (
                  match String.unsafe_get s (pos+6) with
                    | '0' -> (
                        10
                      )
                    | '1' -> (
                        11
                      )
                    | '2' -> (
                        12
                      )
                    | '3' -> (
                        13
                      )
                    | '4' -> (
                        14
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            )
          | 2 ->
            field_field2 := (
              Some (
                (
                  read__string_option
                ) p lb
              )
            );
          | 3 ->
            field_field3 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int64
                ) p lb
              )
            );
          | 4 ->
            field_field4 := (
              Some (
                (
                  read__x_adbef7e
                ) p lb
              )
            );
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field5 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field6 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            )
          | 7 ->
            field_field7 := (
              Some (
                (
                  read_test_variant
                ) p lb
              )
            );
          | 8 ->
            field_field8 := (
              Some (
                (
                  read__x_20d39e2
                ) p lb
              )
            );
          | 9 ->
            field_field9 := (
              Some (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x2 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int8
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x3 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x4 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int32
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x5 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int64
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1, x2, x3, x4, x5)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3; 4; 5 ]);
                ) p lb
              )
            );
          | 10 ->
            field_field10 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 11 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field11 := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 12 ->
            field_field12 := (
              Some (
                (
                  read__unit_list
                ) p lb
              )
            );
          | 13 ->
            field_field13 := (
              Some (
                (
                  read__string_option_list
                ) p lb
              )
            );
          | 14 ->
            field_field14 := (
              Some (
                (
                  read_date
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
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                    match String.unsafe_get s (pos+5) with
                      | '0' -> (
                          0
                        )
                      | '1' -> (
                          1
                        )
                      | '2' -> (
                          2
                        )
                      | '3' -> (
                          3
                        )
                      | '4' -> (
                          4
                        )
                      | '5' -> (
                          5
                        )
                      | '6' -> (
                          6
                        )
                      | '7' -> (
                          7
                        )
                      | '8' -> (
                          8
                        )
                      | '9' -> (
                          9
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '1' then (
                    match String.unsafe_get s (pos+6) with
                      | '0' -> (
                          10
                        )
                      | '1' -> (
                          11
                        )
                      | '2' -> (
                          12
                        )
                      | '3' -> (
                          13
                        )
                      | '4' -> (
                          14
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field0 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_int
                    ) p lb
                  )
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field1 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_number
                    ) p lb
                  )
                );
              )
            | 2 ->
              field_field2 := (
                Some (
                  (
                    read__string_option
                  ) p lb
                )
              );
            | 3 ->
              field_field3 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int64
                  ) p lb
                )
              );
            | 4 ->
              field_field4 := (
                Some (
                  (
                    read__x_adbef7e
                  ) p lb
                )
              );
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field5 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_bool
                    ) p lb
                  )
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field6 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 7 ->
              field_field7 := (
                Some (
                  (
                    read_test_variant
                  ) p lb
                )
              );
            | 8 ->
              field_field8 := (
                Some (
                  (
                    read__x_20d39e2
                  ) p lb
                )
              );
            | 9 ->
              field_field9 := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x2 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int8
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x3 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x4 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int32
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x5 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int64
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1, x2, x3, x4, x5)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3; 4; 5 ]);
                  ) p lb
                )
              );
            | 10 ->
              field_field10 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 11 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field11 := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 12 ->
              field_field12 := (
                Some (
                  (
                    read__unit_list
                  ) p lb
                )
              );
            | 13 ->
              field_field13 := (
                Some (
                  (
                    read__string_option_list
                  ) p lb
                )
              );
            | 14 ->
              field_field14 := (
                Some (
                  (
                    read_date
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
            field0 = !field_field0;
            field1 = !field_field1;
            field2 = (match !field_field2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field2");
            field3 = (match !field_field3 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field3");
            field4 = (match !field_field4 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field4");
            field5 = !field_field5;
            field6 = !field_field6;
            field7 = (match !field_field7 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field7");
            field8 = (match !field_field8 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field8");
            field9 = (match !field_field9 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field9");
            field10 = (match !field_field10 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field10");
            field11 = !field_field11;
            field12 = (match !field_field12 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field12");
            field13 = (match !field_field13 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field13");
            field14 = (match !field_field14 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field14");
          }
         : mixed_record)
      )
)
let mixed_record_of_string s =
  read_mixed_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_d88f1c8 = (
  Atdgen_runtime.Oj_run.write_array (
    write_mixed_record
  )
)
let string_of__x_d88f1c8 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_d88f1c8 ob x;
  Buffer.contents ob
let read__x_d88f1c8 = (
  Atdgen_runtime.Oj_run.read_array (
    read_mixed_record
  )
)
let _x_d88f1c8_of_string s =
  read__x_d88f1c8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_7de077c = (
  Atdgen_runtime.Oj_run.write_array (
    write_mixed_record
  )
)
let string_of__x_7de077c ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_7de077c ob x;
  Buffer.contents ob
let read__x_7de077c = (
  Atdgen_runtime.Oj_run.read_array (
    read_mixed_record
  )
)
let _x_7de077c_of_string s =
  read__x_7de077c (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_c393fa9 = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Buffer.add_char ob '[';
      (let x, _ = x in
      (
        write__x_d88f1c8
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write__x_7de077c
      ) ob x
      );
      Buffer.add_char ob ']';
  )
)
let string_of__x_c393fa9 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_c393fa9 ob x;
  Buffer.contents ob
let read__x_c393fa9 = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              read__x_d88f1c8
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read__x_7de077c
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _x_c393fa9_of_string s =
  read__x_c393fa9 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_mixed = (
  write__x_c393fa9
)
let string_of_mixed ?(len = 1024) x =
  let ob = Buffer.create len in
  write_mixed ob x;
  Buffer.contents ob
let read_mixed = (
  read__x_c393fa9
)
let mixed_of_string s =
  read_mixed (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__mixed_record_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_mixed_record
  )
)
let string_of__mixed_record_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__mixed_record_list ob x;
  Buffer.contents ob
let read__mixed_record_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_mixed_record
  )
)
let _mixed_record_list_of_string s =
  read__mixed_record_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_test : _ -> test -> _ = (
  fun ob (x : test) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    (match x.x0 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"x0\":";
      (
        Yojson.Safe.write_int
      )
        ob x;
    );
    (match x.x1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"x1\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x2\":";
    (
      write_mixed
    )
      ob x.x2;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x3\":";
    (
      write__mixed_record_list
    )
      ob x.x3;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x4\":";
    (
      Atdgen_runtime.Oj_run.write_int64
    )
      ob x.x4;
    Buffer.add_char ob '}';
)
let string_of_test ?(len = 1024) x =
  let ob = Buffer.create len in
  write_test ob x;
  Buffer.contents ob
let read_test = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x0 = ref (None) in
    let field_x1 = ref (None) in
    let field_x2 = ref (None) in
    let field_x3 = ref (None) in
    let field_x4 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 2 && String.unsafe_get s pos = 'x' then (
            match String.unsafe_get s (pos+1) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | '2' -> (
                  2
                )
              | '3' -> (
                  3
                )
              | '4' -> (
                  4
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
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_x0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_x1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            )
          | 2 ->
            field_x2 := (
              Some (
                (
                  read_mixed
                ) p lb
              )
            );
          | 3 ->
            field_x3 := (
              Some (
                (
                  read__mixed_record_list
                ) p lb
              )
            );
          | 4 ->
            field_x4 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int64
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
            if len = 2 && String.unsafe_get s pos = 'x' then (
              match String.unsafe_get s (pos+1) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | '2' -> (
                    2
                  )
                | '3' -> (
                    3
                  )
                | '4' -> (
                    4
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
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_x0 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_int
                    ) p lb
                  )
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_x1 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_number
                    ) p lb
                  )
                );
              )
            | 2 ->
              field_x2 := (
                Some (
                  (
                    read_mixed
                  ) p lb
                )
              );
            | 3 ->
              field_x3 := (
                Some (
                  (
                    read__mixed_record_list
                  ) p lb
                )
              );
            | 4 ->
              field_x4 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int64
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
            x0 = !field_x0;
            x1 = !field_x1;
            x2 = (match !field_x2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x2");
            x3 = (match !field_x3 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x3");
            x4 = (match !field_x4 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x4");
          }
         : test)
      )
)
let test_of_string s =
  read_test (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tup = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x = x in
    (
      write_test
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_tup ?(len = 1024) x =
  let ob = Buffer.create len in
  write_tup ob x;
  Buffer.contents ob
let read_tup = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read_test
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let tup_of_string s =
  read_tup (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_test_field_prefix : _ -> test_field_prefix -> _ = (
  fun ob (x : test_field_prefix) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"hello\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.theprefix_hello;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"world\":";
    (
      Yojson.Safe.write_int
    )
      ob x.theprefix_world;
    Buffer.add_char ob '}';
)
let string_of_test_field_prefix ?(len = 1024) x =
  let ob = Buffer.create len in
  write_test_field_prefix ob x;
  Buffer.contents ob
let read_test_field_prefix = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_theprefix_hello = ref (None) in
    let field_theprefix_world = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 then (
            match String.unsafe_get s pos with
              | 'h' -> (
                  if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'o' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 'w' -> (
                  if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
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
            field_theprefix_hello := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 1 ->
            field_theprefix_world := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
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
            if len = 5 then (
              match String.unsafe_get s pos with
                | 'h' -> (
                    if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'o' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 'w' -> (
                    if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                      1
                    )
                    else (
                      -1
                    )
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
              field_theprefix_hello := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 1 ->
              field_theprefix_world := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
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
            theprefix_hello = (match !field_theprefix_hello with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "theprefix_hello");
            theprefix_world = (match !field_theprefix_world with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "theprefix_world");
          }
         : test_field_prefix)
      )
)
let test_field_prefix_of_string s =
  read_test_field_prefix (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_star_rating = (
  Yojson.Safe.write_int
)
let string_of_star_rating ?(len = 1024) x =
  let ob = Buffer.create len in
  write_star_rating ob x;
  Buffer.contents ob
let read_star_rating = (
  Atdgen_runtime.Oj_run.read_int
)
let star_rating_of_string s =
  read_star_rating (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_generic : _ -> _ generic -> _ = (
  fun ob (x : _ generic) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x294623\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x294623;
    Buffer.add_char ob '}';
)
let string_of__string_generic ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_generic ob x;
  Buffer.contents ob
let read__string_generic = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x294623 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
            0
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
            field_x294623 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
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
            if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
              0
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
              field_x294623 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
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
            x294623 = (match !field_x294623 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x294623");
          }
         : _ generic)
      )
)
let _string_generic_of_string s =
  read__string_generic (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_specialized = (
  write__string_generic
)
let string_of_specialized ?(len = 1024) x =
  let ob = Buffer.create len in
  write_specialized ob x;
  Buffer.contents ob
let read_specialized = (
  read__string_generic
)
let specialized_of_string s =
  read_specialized (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_some_record : _ -> some_record -> _ = (
  fun ob (x : some_record) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"some_field\":";
    (
      Yojson.Safe.write_int
    )
      ob x.some_field;
    Buffer.add_char ob '}';
)
let string_of_some_record ?(len = 1024) x =
  let ob = Buffer.create len in
  write_some_record ob x;
  Buffer.contents ob
let read_some_record = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_some_field = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 10 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'd' then (
            0
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
            field_some_field := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
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
            if len = 10 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'd' then (
              0
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
              field_some_field := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
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
            some_field = (match !field_some_field with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "some_field");
          }
         : some_record)
      )
)
let some_record_of_string s =
  read_some_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_precision : _ -> precision -> _ = (
  fun ob (x : precision) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"sqrt2_5\":";
    (
      Yojson.Safe.write_std_float_prec 5
    )
      ob x.sqrt2_5;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"small_2\":";
    (
      Yojson.Safe.write_std_float_prec 2
    )
      ob x.small_2;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"large_2\":";
    (
      Yojson.Safe.write_std_float_prec 2
    )
      ob x.large_2;
    Buffer.add_char ob '}';
)
let string_of_precision ?(len = 1024) x =
  let ob = Buffer.create len in
  write_precision ob x;
  Buffer.contents ob
let read_precision = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_sqrt2_5 = ref (None) in
    let field_small_2 = ref (None) in
    let field_large_2 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 7 then (
            match String.unsafe_get s pos with
              | 'l' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 's' -> (
                  match String.unsafe_get s (pos+1) with
                    | 'm' -> (
                        if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'q' -> (
                        if String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '2' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '5' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
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
            field_sqrt2_5 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 1 ->
            field_small_2 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 2 ->
            field_large_2 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
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
            if len = 7 then (
              match String.unsafe_get s pos with
                | 'l' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                      2
                    )
                    else (
                      -1
                    )
                  )
                | 's' -> (
                    match String.unsafe_get s (pos+1) with
                      | 'm' -> (
                          if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                            1
                          )
                          else (
                            -1
                          )
                        )
                      | 'q' -> (
                          if String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '2' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '5' then (
                            0
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
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
              field_sqrt2_5 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 1 ->
              field_small_2 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 2 ->
              field_large_2 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
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
            sqrt2_5 = (match !field_sqrt2_5 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "sqrt2_5");
            small_2 = (match !field_small_2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "small_2");
            large_2 = (match !field_large_2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "large_2");
          }
         : precision)
      )
)
let precision_of_string s =
  read_precision (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_p'' = (
  write__int_p
)
let string_of_p'' ?(len = 1024) x =
  let ob = Buffer.create len in
  write_p'' ob x;
  Buffer.contents ob
let read_p'' = (
  read__int_p
)
let p''_of_string s =
  read_p'' (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_bee1b88 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_int
  )
)
let string_of__x_bee1b88 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_bee1b88 ob x;
  Buffer.contents ob
let read__x_bee1b88 = (
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
                  Atdgen_runtime.Oj_run.read_int
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
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _x_bee1b88_of_string s =
  read__x_bee1b88 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_option_validation = (
  write__x_bee1b88
)
let string_of_option_validation ?(len = 1024) x =
  let ob = Buffer.create len in
  write_option_validation ob x;
  Buffer.contents ob
let read_option_validation = (
  read__x_bee1b88
)
let option_validation_of_string s =
  read_option_validation (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__some_record_wrap = (
  write_some_record
)
let string_of__some_record_wrap ?(len = 1024) x =
  let ob = Buffer.create len in
  write__some_record_wrap ob x;
  Buffer.contents ob
let read__some_record_wrap = (
  read_some_record
)
let _some_record_wrap_of_string s =
  read__some_record_wrap (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_no_real_wrap = (
  write__some_record_wrap
)
let string_of_no_real_wrap ?(len = 1024) x =
  let ob = Buffer.create len in
  write_no_real_wrap ob x;
  Buffer.contents ob
let read_no_real_wrap = (
  read__some_record_wrap
)
let no_real_wrap_of_string s =
  read_no_real_wrap (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_e48509c = (
  fun ob x -> (
    let x = ( Test_lib.Natural.unwrap ) x in (
      Yojson.Safe.write_int
    ) ob x)
)
let string_of__x_e48509c ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_e48509c ob x;
  Buffer.contents ob
let read__x_e48509c = (
  fun p lb ->
    let x = (
      Atdgen_runtime.Oj_run.read_int
    ) p lb in
    ( Test_lib.Natural.wrap ) x
)
let _x_e48509c_of_string s =
  read__x_e48509c (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_natural = (
  write__x_e48509c
)
let string_of_natural ?(len = 1024) x =
  let ob = Buffer.create len in
  write_natural ob x;
  Buffer.contents ob
let read_natural = (
  read__x_e48509c
)
let natural_of_string s =
  read_natural (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_2596d76 = (
  fun ob x -> (
    let x = ( function `Id s -> s ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__x_2596d76 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_2596d76 ob x;
  Buffer.contents ob
let read__x_2596d76 = (
  fun p lb ->
    let x = (
      Atdgen_runtime.Oj_run.read_string
    ) p lb in
    ( fun s -> `Id s ) x
)
let _x_2596d76_of_string s =
  read__x_2596d76 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_id = (
  write__x_2596d76
)
let string_of_id ?(len = 1024) x =
  let ob = Buffer.create len in
  write_id ob x;
  Buffer.contents ob
let read_id = (
  read__x_2596d76
)
let id_of_string s =
  read_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_b6e4b4c = (
  Atdgen_runtime.Oj_run.write_assoc_list (
    write_id
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__x_b6e4b4c ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_b6e4b4c ob x;
  Buffer.contents ob
let read__x_b6e4b4c = (
  Atdgen_runtime.Oj_run.read_assoc_list (
    read_id
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _x_b6e4b4c_of_string s =
  read__x_b6e4b4c (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_json_map = (
  write__x_b6e4b4c
)
let string_of_json_map ?(len = 1024) x =
  let ob = Buffer.create len in
  write_json_map ob x;
  Buffer.contents ob
let read_json_map = (
  read__x_b6e4b4c
)
let json_map_of_string s =
  read_json_map (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_intopt = (
  write__int_option
)
let string_of_intopt ?(len = 1024) x =
  let ob = Buffer.create len in
  write_intopt ob x;
  Buffer.contents ob
let read_intopt = (
  read__int_option
)
let intopt_of_string s =
  read_intopt (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_547263f = (
  Atdgen_runtime.Oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__x_547263f ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_547263f ob x;
  Buffer.contents ob
let read__x_547263f = (
  Atdgen_runtime.Oj_run.read_assoc_list (
    Atdgen_runtime.Oj_run.read_string
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _x_547263f_of_string s =
  read__x_547263f (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int_assoc_list = (
  write__x_547263f
)
let string_of_int_assoc_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int_assoc_list ob x;
  Buffer.contents ob
let read_int_assoc_list = (
  read__x_547263f
)
let int_assoc_list_of_string s =
  read_int_assoc_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_0a94e5e = (
  Atdgen_runtime.Oj_run.write_assoc_array (
    Yojson.Safe.write_string
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__x_0a94e5e ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_0a94e5e ob x;
  Buffer.contents ob
let read__x_0a94e5e = (
  Atdgen_runtime.Oj_run.read_assoc_array (
    Atdgen_runtime.Oj_run.read_string
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _x_0a94e5e_of_string s =
  read__x_0a94e5e (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int_assoc_array = (
  write__x_0a94e5e
)
let string_of_int_assoc_array ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int_assoc_array ob x;
  Buffer.contents ob
let read_int_assoc_array = (
  read__x_0a94e5e
)
let int_assoc_array_of_string s =
  read_int_assoc_array (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int8 = (
  Yojson.Safe.write_int
)
let string_of_int8 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int8 ob x;
  Buffer.contents ob
let read_int8 = (
  Atdgen_runtime.Oj_run.read_int
)
let int8_of_string s =
  read_int8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int64 = (
  Atdgen_runtime.Oj_run.write_int64
)
let string_of_int64 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int64 ob x;
  Buffer.contents ob
let read_int64 = (
  Atdgen_runtime.Oj_run.read_int64
)
let int64_of_string s =
  read_int64 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int32 = (
  Atdgen_runtime.Oj_run.write_int32
)
let string_of_int32 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int32 ob x;
  Buffer.contents ob
let read_int32 = (
  Atdgen_runtime.Oj_run.read_int32
)
let int32_of_string s =
  read_int32 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_hello = (
  fun ob x ->
    match x with
      | `Hello x ->
        Buffer.add_string ob "[\"Hello\",";
        (
          Yojson.Safe.write_string
        ) ob x;
        Buffer.add_char ob ']'
      | `World -> Buffer.add_string ob "\"World\""
)
let string_of_hello ?(len = 1024) x =
  let ob = Buffer.create len in
  write_hello ob x;
  Buffer.contents ob
let read_hello = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Hello" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Hello x
            | "World" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `World
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "World" ->
              `World
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Hello" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Hello x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let hello_of_string s =
  read_hello (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_generic write__a : _ -> 'a generic -> _ = (
  fun ob (x : 'a generic) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"x294623\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x294623;
    Buffer.add_char ob '}';
)
let string_of_generic write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_generic write__a ob x;
  Buffer.contents ob
let read_generic read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x294623 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
            0
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
            field_x294623 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
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
            if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
              0
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
              field_x294623 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
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
            x294623 = (match !field_x294623 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x294623");
          }
         : 'a generic)
      )
)
let generic_of_string read__a s =
  read_generic read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_floats : _ -> floats -> _ = (
  fun ob (x : floats) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"f32\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f32;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"f64\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f64;
    Buffer.add_char ob '}';
)
let string_of_floats ?(len = 1024) x =
  let ob = Buffer.create len in
  write_floats ob x;
  Buffer.contents ob
let read_floats = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f32 = ref (None) in
    let field_f64 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 3 && String.unsafe_get s pos = 'f' then (
            match String.unsafe_get s (pos+1) with
              | '3' -> (
                  if String.unsafe_get s (pos+2) = '2' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | '6' -> (
                  if String.unsafe_get s (pos+2) = '4' then (
                    1
                  )
                  else (
                    -1
                  )
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
            field_f32 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 1 ->
            field_f64 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
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
            if len = 3 && String.unsafe_get s pos = 'f' then (
              match String.unsafe_get s (pos+1) with
                | '3' -> (
                    if String.unsafe_get s (pos+2) = '2' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | '6' -> (
                    if String.unsafe_get s (pos+2) = '4' then (
                      1
                    )
                    else (
                      -1
                    )
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
              field_f32 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 1 ->
              field_f64 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
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
            f32 = (match !field_f32 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f32");
            f64 = (match !field_f64 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f64");
          }
         : floats)
      )
)
let floats_of_string s =
  read_floats (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_list ob x;
  Buffer.contents ob
let read__string_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _string_list_of_string s =
  read__string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_extended_tuple = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _, _, _, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x, _, _, _, _ = x in
    (
      Yojson.Safe.write_std_float
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, x, _, _, _ = x in
    (
      Yojson.Safe.write_bool
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, _, x, _, _ = x in
    (
      write__int_option
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, _, _, x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, _, _, _, x = x in
    (
      write__string_list
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_extended_tuple ?(len = 1024) x =
  let ob = Buffer.create len in
  write_extended_tuple ob x;
  Buffer.contents ob
let read_extended_tuple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_number
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_bool
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x3 =
        let x =
          (
            read__int_option
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x4 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_string
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      let x5 =
        if !end_of_tuple then ([])
        else (
          let x = (
            (
              read__string_list
            ) p lb
          ) in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple ->
            end_of_tuple := true);
          x
        )
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2, x3, x4, x5)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 4 ]);
)
let extended_tuple_of_string s =
  read_extended_tuple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_extended : _ -> extended -> _ = (
  fun ob (x : extended) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b0\":";
    (
      Yojson.Safe.write_int
    )
      ob x.b0x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b1\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b1x;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b2\":";
    (
      Yojson.Safe.write_string
    )
      ob x.b2x;
    (match x.b3x with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"b3\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b4\":";
    (
      write__string_option
    )
      ob x.b4x;
    if x.b5x <> 0.5 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"b5\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x.b5x;
    );
    Buffer.add_char ob '}';
)
let string_of_extended ?(len = 1024) x =
  let ob = Buffer.create len in
  write_extended ob x;
  Buffer.contents ob
let read_extended = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_b0x = ref (None) in
    let field_b1x = ref (None) in
    let field_b2x = ref (None) in
    let field_b3x = ref (None) in
    let field_b4x = ref (None) in
    let field_b5x = ref (0.5) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 2 && String.unsafe_get s pos = 'b' then (
            match String.unsafe_get s (pos+1) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | '2' -> (
                  2
                )
              | '3' -> (
                  3
                )
              | '4' -> (
                  4
                )
              | '5' -> (
                  5
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
            field_b0x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b1x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 2 ->
            field_b2x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_b3x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            )
          | 4 ->
            field_b4x := (
              Some (
                (
                  read__string_option
                ) p lb
              )
            );
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_b5x := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
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
            if len = 2 && String.unsafe_get s pos = 'b' then (
              match String.unsafe_get s (pos+1) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | '2' -> (
                    2
                  )
                | '3' -> (
                    3
                  )
                | '4' -> (
                    4
                  )
                | '5' -> (
                    5
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
              field_b0x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b1x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 2 ->
              field_b2x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_b3x := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 4 ->
              field_b4x := (
                Some (
                  (
                    read__string_option
                  ) p lb
                )
              );
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_b5x := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            b0x = (match !field_b0x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b0x");
            b1x = (match !field_b1x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b1x");
            b2x = (match !field_b2x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b2x");
            b3x = !field_b3x;
            b4x = (match !field_b4x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b4x");
            b5x = !field_b5x;
          }
         : extended)
      )
)
let extended_of_string s =
  read_extended (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_a08e9e5 = (
  fun ob x -> (
    let x = ( Test_lib.Even_natural.unwrap ) x in (
      write_natural
    ) ob x)
)
let string_of__x_a08e9e5 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_a08e9e5 ob x;
  Buffer.contents ob
let read__x_a08e9e5 = (
  fun p lb ->
    let x = (
      read_natural
    ) p lb in
    ( Test_lib.Even_natural.wrap ) x
)
let _x_a08e9e5_of_string s =
  read__x_a08e9e5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_even_natural = (
  write__x_a08e9e5
)
let string_of_even_natural ?(len = 1024) x =
  let ob = Buffer.create len in
  write_even_natural ob x;
  Buffer.contents ob
let read_even_natural = (
  read__x_a08e9e5
)
let even_natural_of_string s =
  read_even_natural (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_def = (
  Test_lib.Json.write_def
)
let string_of_def ?(len = 1024) x =
  let ob = Buffer.create len in
  write_def ob x;
  Buffer.contents ob
let read_def = (
  Test_lib.Json.read_def
)
let def_of_string s =
  read_def (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_char = (
  Atdgen_runtime.Oj_run.write_int8
)
let string_of_char ?(len = 1024) x =
  let ob = Buffer.create len in
  write_char ob x;
  Buffer.contents ob
let read_char = (
  Atdgen_runtime.Oj_run.read_int8
)
let char_of_string s =
  read_char (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_base_tuple = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x = x in
    (
      Yojson.Safe.write_std_float
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_base_tuple ?(len = 1024) x =
  let ob = Buffer.create len in
  write_base_tuple ob x;
  Buffer.contents ob
let read_base_tuple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_number
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let base_tuple_of_string s =
  read_base_tuple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_base : _ -> base -> _ = (
  fun ob (x : base) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b0\":";
    (
      Yojson.Safe.write_int
    )
      ob x.b0;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"b1\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b1;
    Buffer.add_char ob '}';
)
let string_of_base ?(len = 1024) x =
  let ob = Buffer.create len in
  write_base ob x;
  Buffer.contents ob
let read_base = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_b0 = ref (None) in
    let field_b1 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 2 && String.unsafe_get s pos = 'b' then (
            match String.unsafe_get s (pos+1) with
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
            field_b0 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b1 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
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
            if len = 2 && String.unsafe_get s pos = 'b' then (
              match String.unsafe_get s (pos+1) with
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
              field_b0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
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
            b0 = (match !field_b0 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b0");
            b1 = (match !field_b1 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b1");
          }
         : base)
      )
)
let base_of_string s =
  read_base (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__x_f9e3589 write__a = (
  Atdgen_runtime.Oj_run.write_array (
    write__a
  )
)
let string_of__x_f9e3589 write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_f9e3589 write__a ob x;
  Buffer.contents ob
let read__x_f9e3589 read__a = (
  Atdgen_runtime.Oj_run.read_array (
    read__a
  )
)
let _x_f9e3589_of_string read__a s =
  read__x_f9e3589 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_array write__a = (
  write__x_f9e3589 write__a
)
let string_of_array write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_array write__a ob x;
  Buffer.contents ob
let read_array read__a = (
  read__x_f9e3589 read__a
)
let array_of_string read__a s =
  read_array read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs3 write__a = (
  write__a_list write__a
)
let string_of_abs3 write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_abs3 write__a ob x;
  Buffer.contents ob
let read_abs3 read__a = (
  read__a_list read__a
)
let abs3_of_string read__a s =
  read_abs3 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs2 write__a = (
  write__a_list write__a
)
let string_of_abs2 write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_abs2 write__a ob x;
  Buffer.contents ob
let read_abs2 read__a = (
  read__a_list read__a
)
let abs2_of_string read__a s =
  read_abs2 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs1 write__a = (
  write__a_list write__a
)
let string_of_abs1 write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_abs1 write__a ob x;
  Buffer.contents ob
let read_abs1 read__a = (
  read__a_list read__a
)
let abs1_of_string read__a s =
  read_abs1 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let create_r 
  ~a
  ~b
  ~c
  () : r =
  {
    a = a;
    b = b;
    c = c;
  }
let create_poly 
  ~fst
  ~snd
  () : ('x, 'y) poly =
  {
    fst = fst;
    snd = snd;
  }
let create_val1 
  ~val1_x
  () : val1 =
  {
    val1_x = val1_x;
  }
let create_val2 
  ~val2_x
  ?val2_y
  () : val2 =
  {
    val2_x = val2_x;
    val2_y = val2_y;
  }
let create_mixed_record 
  ?field0
  ?field1
  ~field2
  ~field3
  ~field4
  ?field5
  ?field6
  ~field7
  ~field8
  ~field9
  ~field10
  ?(field11 = false)
  ~field12
  ~field13
  ~field14
  () : mixed_record =
  {
    field0 = field0;
    field1 = field1;
    field2 = field2;
    field3 = field3;
    field4 = field4;
    field5 = field5;
    field6 = field6;
    field7 = field7;
    field8 = field8;
    field9 = field9;
    field10 = field10;
    field11 = field11;
    field12 = field12;
    field13 = field13;
    field14 = field14;
  }
let create_test 
  ?x0
  ?x1
  ~x2
  ~x3
  ~x4
  () : test =
  {
    x0 = x0;
    x1 = x1;
    x2 = x2;
    x3 = x3;
    x4 = x4;
  }
let create_test_field_prefix 
  ~theprefix_hello
  ~theprefix_world
  () : test_field_prefix =
  {
    theprefix_hello = theprefix_hello;
    theprefix_world = theprefix_world;
  }
let create_some_record 
  ~some_field
  () : some_record =
  {
    some_field = some_field;
  }
let create_precision 
  ~sqrt2_5
  ~small_2
  ~large_2
  () : precision =
  {
    sqrt2_5 = sqrt2_5;
    small_2 = small_2;
    large_2 = large_2;
  }
let create_generic 
  ~x294623
  () : 'a generic =
  {
    x294623 = x294623;
  }
let create_floats 
  ~f32
  ~f64
  () : floats =
  {
    f32 = f32;
    f64 = f64;
  }
let create_extended 
  ~b0x
  ~b1x
  ~b2x
  ?b3x
  ~b4x
  ?(b5x = 0.5)
  () : extended =
  {
    b0x = b0x;
    b1x = b1x;
    b2x = b2x;
    b3x = b3x;
    b4x = b4x;
    b5x = b5x;
  }
let create_base 
  ~b0
  ~b1
  () : base =
  {
    b0 = b0;
    b1 = b1;
  }
