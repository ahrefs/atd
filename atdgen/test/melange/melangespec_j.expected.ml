(* Auto-generated from "melangespec.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type recurse = Melangespec_t.recurse = { recurse_items: recurse list }

type mutual_recurse1 = Melangespec_t.mutual_recurse1 = {
  mutual_recurse2: mutual_recurse2
}

and mutual_recurse2 = Melangespec_t.mutual_recurse2 = {
  mutual_recurse1: mutual_recurse1
}

type with_open_enum = Melangespec_t.with_open_enum

type with_open_enum_list = Melangespec_t.with_open_enum_list

type variant3 = Melangespec_t.variant3 =  C | B | A of string 

type variant2 = Melangespec_t.variant2 =  A | C 

type variant1 = Melangespec_t.variant1 =  A of string | B 

type valid = Melangespec_t.valid

type v2 = Melangespec_t.v2 =  V1_foo of int | V2_bar of bool 

type v1 = Melangespec_t.v1 =  V1_foo of bool | V2_bar of int 

type using_object = Melangespec_t.using_object = { f: (string * int) list }

type single_tuple = Melangespec_t.single_tuple

type id = Melangespec_t.id

type 'a simple_var = 'a Melangespec_t.simple_var

type simple_vars = Melangespec_t.simple_vars

type 'a same_pair = 'a Melangespec_t.same_pair

type record_json_name = Melangespec_t.record_json_name = { foo: int }

type point = Melangespec_t.point

type 'a param_similar = 'a Melangespec_t.param_similar = {
  data: 'a;
  something: int
}

type 'a param = 'a Melangespec_t.param = { data: 'a; nothing: unit }

type ('a, 'b) pair = ('a, 'b) Melangespec_t.pair = { left: 'a; right: 'b }

type 'a pairs = 'a Melangespec_t.pairs

type label = Melangespec_t.label

type labeled = Melangespec_t.labeled = { flag: valid; lb: label; count: int }

type from_module_a = A_t.from_module_a

type b = Melangespec_t.b = { thing: int }

type a = Melangespec_t.a = { thing: string; other_thing: bool }

type adapted = Melangespec_t.adapted

let rec write_mutual_recurse1 : _ -> mutual_recurse1 -> _ = (
  fun ob (x : mutual_recurse1) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"mutual_recurse2\":";
    (
      write_mutual_recurse2
    )
      ob x.mutual_recurse2;
    Buffer.add_char ob '}';
)
and string_of_mutual_recurse1 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_mutual_recurse1 ob x;
  Buffer.contents ob
and write_mutual_recurse2 : _ -> mutual_recurse2 -> _ = (
  fun ob (x : mutual_recurse2) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"mutual_recurse1\":";
    (
      write_mutual_recurse1
    )
      ob x.mutual_recurse1;
    Buffer.add_char ob '}';
)
and string_of_mutual_recurse2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_mutual_recurse2 ob x;
  Buffer.contents ob
let rec read_mutual_recurse1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_mutual_recurse2 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 15 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '2' then (
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
            field_mutual_recurse2 := (
              Some (
                (
                  read_mutual_recurse2
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
            if len = 15 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '2' then (
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
              field_mutual_recurse2 := (
                Some (
                  (
                    read_mutual_recurse2
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
            mutual_recurse2 = (match !field_mutual_recurse2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "mutual_recurse2");
          }
         : mutual_recurse1)
      )
)
and mutual_recurse1_of_string s =
  read_mutual_recurse1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_mutual_recurse2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_mutual_recurse1 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 15 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '1' then (
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
            field_mutual_recurse1 := (
              Some (
                (
                  read_mutual_recurse1
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
            if len = 15 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'u' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '1' then (
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
              field_mutual_recurse1 := (
                Some (
                  (
                    read_mutual_recurse1
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
            mutual_recurse1 = (match !field_mutual_recurse1 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "mutual_recurse1");
          }
         : mutual_recurse2)
      )
)
and mutual_recurse2_of_string s =
  read_mutual_recurse2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__recurse_list ob x = (
  Atdgen_runtime.Oj_run.write_list (
    write_recurse
  )
) ob x
and string_of__recurse_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__recurse_list ob x;
  Buffer.contents ob
and write_recurse : _ -> recurse -> _ = (
  fun ob (x : recurse) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"recurse_items\":";
    (
      write__recurse_list
    )
      ob x.recurse_items;
    Buffer.add_char ob '}';
)
and string_of_recurse ?(len = 1024) x =
  let ob = Buffer.create len in
  write_recurse ob x;
  Buffer.contents ob
let rec read__recurse_list p lb = (
  Atdgen_runtime.Oj_run.read_list (
    read_recurse
  )
) p lb
and _recurse_list_of_string s =
  read__recurse_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_recurse = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_recurse_items = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 13 && String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 's' then (
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
            field_recurse_items := (
              Some (
                (
                  read__recurse_list
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
            if len = 13 && String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 's' then (
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
              field_recurse_items := (
                Some (
                  (
                    read__recurse_list
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
            recurse_items = (match !field_recurse_items with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "recurse_items");
          }
         : recurse)
      )
)
and recurse_of_string s =
  read_recurse (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_with_open_enum = (
  fun ob x ->
    match x with
      | `Alpha -> Buffer.add_string ob "\"Alpha\""
      | `Beta -> Buffer.add_string ob "\"Beta\""
      | `Other x -> (
          Yojson.Safe.write_string
        ) ob x;
)
let string_of_with_open_enum ?(len = 1024) x =
  let ob = Buffer.create len in
  write_with_open_enum ob x;
  Buffer.contents ob
let read_with_open_enum = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Alpha" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Alpha
            | "Beta" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Beta
            | x ->
              `Other x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Alpha" ->
              `Alpha
            | "Beta" ->
              `Beta
            | x ->
              `Other x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let with_open_enum_of_string s =
  read_with_open_enum (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__with_open_enum_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_with_open_enum
  )
)
let string_of__with_open_enum_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__with_open_enum_list ob x;
  Buffer.contents ob
let read__with_open_enum_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_with_open_enum
  )
)
let _with_open_enum_list_of_string s =
  read__with_open_enum_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_with_open_enum_list = (
  write__with_open_enum_list
)
let string_of_with_open_enum_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write_with_open_enum_list ob x;
  Buffer.contents ob
let read_with_open_enum_list = (
  read__with_open_enum_list
)
let with_open_enum_list_of_string s =
  read_with_open_enum_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_variant3 : _ -> variant3 -> _ = (
  fun ob (x : variant3) ->
    match x with
      | C -> Buffer.add_string ob "\"C\""
      | B -> Buffer.add_string ob "\"B\""
      | A x -> (
          Yojson.Safe.write_string
        ) ob x;
)
let string_of_variant3 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_variant3 ob x;
  Buffer.contents ob
let read_variant3 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "C" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (C : variant3)
            | "B" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (B : variant3)
            | x ->
              (A x : variant3)
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "C" ->
              (C : variant3)
            | "B" ->
              (B : variant3)
            | x ->
              (A x : variant3)
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let variant3_of_string s =
  read_variant3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_variant2 : _ -> variant2 -> _ = (
  fun ob (x : variant2) ->
    match x with
      | A -> Buffer.add_string ob "\"A\""
      | C -> Buffer.add_string ob "\"C\""
)
let string_of_variant2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_variant2 ob x;
  Buffer.contents ob
let read_variant2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A : variant2)
            | "C" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (C : variant2)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              (A : variant2)
            | "C" ->
              (C : variant2)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let variant2_of_string s =
  read_variant2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_variant1 : _ -> variant1 -> _ = (
  fun ob (x : variant1) ->
    match x with
      | A x ->
        Buffer.add_string ob "[\"A\",";
        (
          Yojson.Safe.write_string
        ) ob x;
        Buffer.add_char ob ']'
      | B -> Buffer.add_string ob "\"B\""
)
let string_of_variant1 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_variant1 ob x;
  Buffer.contents ob
let read_variant1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A x : variant1)
            | "B" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (B : variant1)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "B" ->
              (B : variant1)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (A x : variant1)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let variant1_of_string s =
  read_variant1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_valid = (
  Yojson.Safe.write_bool
)
let string_of_valid ?(len = 1024) x =
  let ob = Buffer.create len in
  write_valid ob x;
  Buffer.contents ob
let read_valid = (
  Atdgen_runtime.Oj_run.read_bool
)
let valid_of_string s =
  read_valid (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_v2 : _ -> v2 -> _ = (
  fun ob (x : v2) ->
    match x with
      | V1_foo x ->
        Buffer.add_string ob "[\"V1_foo\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Buffer.add_char ob ']'
      | V2_bar x ->
        Buffer.add_string ob "[\"V2_bar\",";
        (
          Yojson.Safe.write_bool
        ) ob x;
        Buffer.add_char ob ']'
)
let string_of_v2 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_v2 ob x;
  Buffer.contents ob
let read_v2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "V1_foo" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (V1_foo x : v2)
            | "V2_bar" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (V2_bar x : v2)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "V1_foo" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (V1_foo x : v2)
            | "V2_bar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (V2_bar x : v2)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let v2_of_string s =
  read_v2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_v1 : _ -> v1 -> _ = (
  fun ob (x : v1) ->
    match x with
      | V1_foo x ->
        Buffer.add_string ob "[\"V1_foo\",";
        (
          Yojson.Safe.write_bool
        ) ob x;
        Buffer.add_char ob ']'
      | V2_bar x ->
        Buffer.add_string ob "[\"V2_bar\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Buffer.add_char ob ']'
)
let string_of_v1 ?(len = 1024) x =
  let ob = Buffer.create len in
  write_v1 ob x;
  Buffer.contents ob
let read_v1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "V1_foo" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (V1_foo x : v1)
            | "V2_bar" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (V2_bar x : v1)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "V1_foo" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (V1_foo x : v1)
            | "V2_bar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (V2_bar x : v1)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let v1_of_string s =
  read_v1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write_using_object : _ -> using_object -> _ = (
  fun ob (x : using_object) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"f\":";
    (
      write__x_547263f
    )
      ob x.f;
    Buffer.add_char ob '}';
)
let string_of_using_object ?(len = 1024) x =
  let ob = Buffer.create len in
  write_using_object ob x;
  Buffer.contents ob
let read_using_object = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 1 && String.unsafe_get s pos = 'f' then (
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
            field_f := (
              Some (
                (
                  read__x_547263f
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
            if len = 1 && String.unsafe_get s pos = 'f' then (
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
              field_f := (
                Some (
                  (
                    read__x_547263f
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
            f = (match !field_f with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f");
          }
         : using_object)
      )
)
let using_object_of_string s =
  read_using_object (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_single_tuple = (
  fun ob x ->
    match x with
      | `Single_tuple x ->
        Buffer.add_string ob "[\"Single_tuple\",";
        (
          fun ob x ->
            Buffer.add_char ob '[';
            (let x = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Buffer.add_char ob ']';
        ) ob x;
        Buffer.add_char ob ']'
)
let string_of_single_tuple ?(len = 1024) x =
  let ob = Buffer.create len in
  write_single_tuple ob x;
  Buffer.contents ob
let read_single_tuple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Single_tuple" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
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
                      (x0)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Single_tuple x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Single_tuple" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
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
                      (x0)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Single_tuple x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let single_tuple_of_string s =
  read_single_tuple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write__unit_simple_var = (
  fun ob x ->
    match x with
      | `Foo x ->
        Buffer.add_string ob "[\"Foo\",";
        (
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
              Yojson.Safe.write_int
            ) ob x
            );
            Buffer.add_char ob ']';
        ) ob x;
        Buffer.add_char ob ']'
      | `Bar -> Buffer.add_string ob "\"Bar\""
      | `Foobar x ->
        Buffer.add_string ob "[\"Foobar\",";
        (
          Yojson.Safe.write_null
        ) ob x;
        Buffer.add_char ob ']'
      | `Foo_id x ->
        Buffer.add_string ob "[\"Foo_id\",";
        (
          write_id
        ) ob x;
        Buffer.add_char ob ']'
)
let string_of__unit_simple_var ?(len = 1024) x =
  let ob = Buffer.create len in
  write__unit_simple_var ob x;
  Buffer.contents ob
let read__unit_simple_var = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Foo" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foo x
            | "Bar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Bar
            | "Foobar" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_null
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foobar x
            | "Foo_id" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_id
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foo_id x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Bar" ->
              `Bar
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Foo" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foo x
            | "Foobar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_null
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foobar x
            | "Foo_id" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_id
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foo_id x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _unit_simple_var_of_string s =
  read__unit_simple_var (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__unit_simple_var_list = (
  Atdgen_runtime.Oj_run.write_list (
    write__unit_simple_var
  )
)
let string_of__unit_simple_var_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__unit_simple_var_list ob x;
  Buffer.contents ob
let read__unit_simple_var_list = (
  Atdgen_runtime.Oj_run.read_list (
    read__unit_simple_var
  )
)
let _unit_simple_var_list_of_string s =
  read__unit_simple_var_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_simple_vars = (
  write__unit_simple_var_list
)
let string_of_simple_vars ?(len = 1024) x =
  let ob = Buffer.create len in
  write_simple_vars ob x;
  Buffer.contents ob
let read_simple_vars = (
  read__unit_simple_var_list
)
let simple_vars_of_string s =
  read_simple_vars (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_simple_var write__a = (
  fun ob x ->
    match x with
      | `Foo x ->
        Buffer.add_string ob "[\"Foo\",";
        (
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
              Yojson.Safe.write_int
            ) ob x
            );
            Buffer.add_char ob ']';
        ) ob x;
        Buffer.add_char ob ']'
      | `Bar -> Buffer.add_string ob "\"Bar\""
      | `Foobar x ->
        Buffer.add_string ob "[\"Foobar\",";
        (
          write__a
        ) ob x;
        Buffer.add_char ob ']'
      | `Foo_id x ->
        Buffer.add_string ob "[\"Foo_id\",";
        (
          write_id
        ) ob x;
        Buffer.add_char ob ']'
)
let string_of_simple_var write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_simple_var write__a ob x;
  Buffer.contents ob
let read_simple_var read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Foo" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foo x
            | "Bar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Bar
            | "Foobar" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foobar x
            | "Foo_id" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_id
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Foo_id x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Bar" ->
              `Bar
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Foo" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foo x
            | "Foobar" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foobar x
            | "Foo_id" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_id
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Foo_id x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let simple_var_of_string read__a s =
  read_simple_var read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_same_pair write__a = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _ = x in
    (
      write__a
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x = x in
    (
      write__a
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_same_pair write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_same_pair write__a ob x;
  Buffer.contents ob
let read_same_pair read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            read__a
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
            read__a
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
let same_pair_of_string read__a s =
  read_same_pair read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_record_json_name : _ -> record_json_name -> _ = (
  fun ob (x : record_json_name) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"bar\":";
    (
      Yojson.Safe.write_int
    )
      ob x.foo;
    Buffer.add_char ob '}';
)
let string_of_record_json_name ?(len = 1024) x =
  let ob = Buffer.create len in
  write_record_json_name ob x;
  Buffer.contents ob
let read_record_json_name = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_foo = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 3 && String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' then (
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
            field_foo := (
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
            if len = 3 && String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' then (
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
              field_foo := (
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
            foo = (match !field_foo with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "foo");
          }
         : record_json_name)
      )
)
let record_json_name_of_string s =
  read_record_json_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_point = (
  fun ob x ->
    Buffer.add_char ob '[';
    (let x, _, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, x, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Buffer.add_char ob ',';
    (let _, _, _, x = x in
    (
      Yojson.Safe.write_null
    ) ob x
    );
    Buffer.add_char ob ']';
)
let string_of_point ?(len = 1024) x =
  let ob = Buffer.create len in
  write_point ob x;
  Buffer.contents ob
let read_point = (
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
            Atdgen_runtime.Oj_run.read_string
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
            Atdgen_runtime.Oj_run.read_null
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
      (x0, x1, x2, x3)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3 ]);
)
let point_of_string s =
  read_point (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_param_similar write__a : _ -> 'a param_similar -> _ = (
  fun ob (x : 'a param_similar) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"data\":";
    (
      write__a
    )
      ob x.data;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"something\":";
    (
      Yojson.Safe.write_int
    )
      ob x.something;
    Buffer.add_char ob '}';
)
let string_of_param_similar write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_param_similar write__a ob x;
  Buffer.contents ob
let read_param_similar read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_data = ref (None) in
    let field_something = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                  0
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'g' then (
                  1
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
            field_data := (
              Some (
                (
                  read__a
                ) p lb
              )
            );
          | 1 ->
            field_something := (
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
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'g' then (
                    1
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
              field_data := (
                Some (
                  (
                    read__a
                  ) p lb
                )
              );
            | 1 ->
              field_something := (
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
            data = (match !field_data with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "data");
            something = (match !field_something with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "something");
          }
         : 'a param_similar)
      )
)
let param_similar_of_string read__a s =
  read_param_similar read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_param write__a : _ -> 'a param -> _ = (
  fun ob (x : 'a param) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"data\":";
    (
      write__a
    )
      ob x.data;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"nothing\":";
    (
      Yojson.Safe.write_null
    )
      ob x.nothing;
    Buffer.add_char ob '}';
)
let string_of_param write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_param write__a ob x;
  Buffer.contents ob
let read_param read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_data = ref (None) in
    let field_nothing = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                  0
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' then (
                  1
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
            field_data := (
              Some (
                (
                  read__a
                ) p lb
              )
            );
          | 1 ->
            field_nothing := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_null
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
              | 4 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' then (
                    1
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
              field_data := (
                Some (
                  (
                    read__a
                  ) p lb
                )
              );
            | 1 ->
              field_nothing := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_null
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
            data = (match !field_data with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "data");
            nothing = (match !field_nothing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "nothing");
          }
         : 'a param)
      )
)
let param_of_string read__a s =
  read_param read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pair write__a write__b : _ -> ('a, 'b) pair -> _ = (
  fun ob (x : ('a, 'b) pair) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"left\":";
    (
      write__a
    )
      ob x.left;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"right\":";
    (
      write__b
    )
      ob x.right;
    Buffer.add_char ob '}';
)
let string_of_pair write__a write__b ?(len = 1024) x =
  let ob = Buffer.create len in
  write_pair write__a write__b ob x;
  Buffer.contents ob
let read_pair read__a read__b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_left = ref (None) in
    let field_right = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 't' then (
                  1
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
            field_left := (
              Some (
                (
                  read__a
                ) p lb
              )
            );
          | 1 ->
            field_right := (
              Some (
                (
                  read__b
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
              | 4 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = 't' then (
                    1
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
              field_left := (
                Some (
                  (
                    read__a
                  ) p lb
                )
              );
            | 1 ->
              field_right := (
                Some (
                  (
                    read__b
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
            left = (match !field_left with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "left");
            right = (match !field_right with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "right");
          }
         : ('a, 'b) pair)
      )
)
let pair_of_string read__a read__b s =
  read_pair read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__a_b_pair_list write__a write__b = (
  Atdgen_runtime.Oj_run.write_list (
    write_pair write__a write__a
  )
)
let string_of__a_b_pair_list write__a write__b ?(len = 1024) x =
  let ob = Buffer.create len in
  write__a_b_pair_list write__a write__b ob x;
  Buffer.contents ob
let read__a_b_pair_list read__a read__b = (
  Atdgen_runtime.Oj_run.read_list (
    read_pair read__a read__a
  )
)
let _a_b_pair_list_of_string read__a read__b s =
  read__a_b_pair_list read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pairs write__a = (
  write__a_b_pair_list write__a write__a
)
let string_of_pairs write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_pairs write__a ob x;
  Buffer.contents ob
let read_pairs read__a = (
  read__a_b_pair_list read__a read__a
)
let pairs_of_string read__a s =
  read_pairs read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_label = (
  Yojson.Safe.write_string
)
let string_of_label ?(len = 1024) x =
  let ob = Buffer.create len in
  write_label ob x;
  Buffer.contents ob
let read_label = (
  Atdgen_runtime.Oj_run.read_string
)
let label_of_string s =
  read_label (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_labeled : _ -> labeled -> _ = (
  fun ob (x : labeled) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"flag\":";
    (
      write_valid
    )
      ob x.flag;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"lb\":";
    (
      write_label
    )
      ob x.lb;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.count;
    Buffer.add_char ob '}';
)
let string_of_labeled ?(len = 1024) x =
  let ob = Buffer.create len in
  write_labeled ob x;
  Buffer.contents ob
let read_labeled = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_flag = ref (None) in
    let field_lb = ref (None) in
    let field_count = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'b' then (
                  1
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                  2
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
            field_flag := (
              Some (
                (
                  read_valid
                ) p lb
              )
            );
          | 1 ->
            field_lb := (
              Some (
                (
                  read_label
                ) p lb
              )
            );
          | 2 ->
            field_count := (
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
            match len with
              | 2 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'b' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'g' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                    2
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
              field_flag := (
                Some (
                  (
                    read_valid
                  ) p lb
                )
              );
            | 1 ->
              field_lb := (
                Some (
                  (
                    read_label
                  ) p lb
                )
              );
            | 2 ->
              field_count := (
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
            flag = (match !field_flag with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "flag");
            lb = (match !field_lb with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "lb");
            count = (match !field_count with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "count");
          }
         : labeled)
      )
)
let labeled_of_string s =
  read_labeled (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_from_module_a = (
  A_j.write_from_module_a
)
let string_of_from_module_a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_from_module_a ob x;
  Buffer.contents ob
let read_from_module_a = (
  A_j.read_from_module_a
)
let from_module_a_of_string s =
  read_from_module_a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_b : _ -> b -> _ = (
  fun ob (x : b) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_int
    )
      ob x.thing;
    Buffer.add_char ob '}';
)
let string_of_b ?(len = 1024) x =
  let ob = Buffer.create len in
  write_b ob x;
  Buffer.contents ob
let read_b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
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
            field_thing := (
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
            if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
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
              field_thing := (
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
            thing = (match !field_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "thing");
          }
         : b)
      )
)
let b_of_string s =
  read_b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_a : _ -> a -> _ = (
  fun ob (x : a) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_string
    )
      ob x.thing;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"other_thing\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.other_thing;
    Buffer.add_char ob '}';
)
let string_of_a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_a ob x;
  Buffer.contents ob
let read_a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (None) in
    let field_other_thing = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
                  0
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'g' then (
                  1
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
            field_thing := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_other_thing := (
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
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'g' then (
                    1
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
              field_thing := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_other_thing := (
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
            thing = (match !field_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "thing");
            other_thing = (match !field_other_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "other_thing");
          }
         : a)
      )
)
let a_of_string s =
  read_a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_adapted = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_codec_runtime.Json_adapter.Type_field.restore (
    fun ob x ->
      match x with
        | `A x ->
          Buffer.add_string ob "[\"A\",";
          (
            write_a
          ) ob x;
          Buffer.add_char ob ']'
        | `B x ->
          Buffer.add_string ob "[\"B\",";
          (
            write_b
          ) ob x;
          Buffer.add_char ob ']'
  )
)
let string_of_adapted ?(len = 1024) x =
  let ob = Buffer.create len in
  write_adapted ob x;
  Buffer.contents ob
let read_adapted = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_codec_runtime.Json_adapter.Type_field.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "A" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_a
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `A x
              | "B" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_b
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "A" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_a
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `A x
              | "B" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_b
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let adapted_of_string s =
  read_adapted (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
