(* Auto-generated from "bucklespec.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type valid = Bucklespec_t.valid

type v2 = Bucklespec_t.v2 =  V1_foo of int | V2_bar of bool 

type v1 = Bucklespec_t.v1 =  V1_foo of bool | V2_bar of int 

type id = Bucklespec_t.id

type 'a simple_var = 'a Bucklespec_t.simple_var

type simple_vars = Bucklespec_t.simple_vars

type 'a same_pair = 'a Bucklespec_t.same_pair

type point = Bucklespec_t.point

type 'a param_similar = 'a Bucklespec_t.param_similar = {
  data: 'a;
  something: int
}

type 'a param = 'a Bucklespec_t.param = { data: 'a; nothing: unit }

type ('a, 'b) pair = ('a, 'b) Bucklespec_t.pair = { left: 'a; right: 'b }

type 'a pairs = 'a Bucklespec_t.pairs

type label = Bucklespec_t.label

type labeled = Bucklespec_t.labeled = { flag: valid; lb: label; count: int }

type from_module_a = A_t.from_module_a

let write_valid = (
  Yojson.Safe.write_bool
)
let string_of_valid ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_valid ob x;
  Bi_outbuf.contents ob
let read_valid = (
  Atdgen_runtime.Oj_run.read_bool
)
let valid_of_string s =
  read_valid (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_v2 : _ -> v2 -> _ = (
  fun ob x ->
    match x with
      | V1_foo x ->
        Bi_outbuf.add_string ob "[\"V1_foo\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | V2_bar x ->
        Bi_outbuf.add_string ob "[\"V2_bar\",";
        (
          Yojson.Safe.write_bool
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_v2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_v2 ob x;
  Bi_outbuf.contents ob
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
  fun ob x ->
    match x with
      | V1_foo x ->
        Bi_outbuf.add_string ob "[\"V1_foo\",";
        (
          Yojson.Safe.write_bool
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | V2_bar x ->
        Bi_outbuf.add_string ob "[\"V2_bar\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_v1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_v1 ob x;
  Bi_outbuf.contents ob
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
let write__2 = (
  fun ob x -> (
    let x = ( function `Id s -> s ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    let x = (
      Atdgen_runtime.Oj_run.read_string
    ) p lb in
    ( fun s -> `Id s ) x
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_id = (
  write__2
)
let string_of_id ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_id ob x;
  Bi_outbuf.contents ob
let read_id = (
  read__2
)
let id_of_string s =
  read_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  fun ob x ->
    match x with
      | `Foo x ->
        Bi_outbuf.add_string ob "[\"Foo\",";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '[';
            (let x, _ = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Bi_outbuf.add_char ob ']';
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Bar -> Bi_outbuf.add_string ob "\"Bar\""
      | `Foobar x ->
        Bi_outbuf.add_string ob "[\"Foobar\",";
        (
          Yojson.Safe.write_null
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Foo_id x ->
        Bi_outbuf.add_string ob "[\"Foo_id\",";
        (
          write_id
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
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
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Atdgen_runtime.Oj_run.write_list (
    write__3
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Atdgen_runtime.Oj_run.read_list (
    read__3
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_simple_vars = (
  write__4
)
let string_of_simple_vars ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_simple_vars ob x;
  Bi_outbuf.contents ob
let read_simple_vars = (
  read__4
)
let simple_vars_of_string s =
  read_simple_vars (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_simple_var write__a = (
  fun ob x ->
    match x with
      | `Foo x ->
        Bi_outbuf.add_string ob "[\"Foo\",";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '[';
            (let x, _ = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Bi_outbuf.add_char ob ']';
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Bar -> Bi_outbuf.add_string ob "\"Bar\""
      | `Foobar x ->
        Bi_outbuf.add_string ob "[\"Foobar\",";
        (
          write__a
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Foo_id x ->
        Bi_outbuf.add_string ob "[\"Foo_id\",";
        (
          write_id
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_simple_var write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_simple_var write__a ob x;
  Bi_outbuf.contents ob
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
    Bi_outbuf.add_char ob '[';
    (let x, _ = x in
    (
      write__a
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      write__a
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_same_pair write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_same_pair write__a ob x;
  Bi_outbuf.contents ob
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
let write_point = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, _, x = x in
    (
      Yojson.Safe.write_null
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_point ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_point ob x;
  Bi_outbuf.contents ob
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
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"data\":";
    (
      write__a
    )
      ob x.data;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"something\":";
    (
      Yojson.Safe.write_int
    )
      ob x.something;
    Bi_outbuf.add_char ob '}';
)
let string_of_param_similar write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_param_similar write__a ob x;
  Bi_outbuf.contents ob
let read_param_similar read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_something = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
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
              (
                read__a
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_something := (
              (
                Atdgen_runtime.Oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
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
              invalid_arg "out-of-bounds substring position or length";
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
                (
                  read__a
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_something := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "data"; "something" |];
        (
          {
            data = !field_data;
            something = !field_something;
          }
         : 'a param_similar)
      )
)
let param_similar_of_string read__a s =
  read_param_similar read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_param write__a : _ -> 'a param -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"data\":";
    (
      write__a
    )
      ob x.data;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"nothing\":";
    (
      Yojson.Safe.write_null
    )
      ob x.nothing;
    Bi_outbuf.add_char ob '}';
)
let string_of_param write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_param write__a ob x;
  Bi_outbuf.contents ob
let read_param read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_nothing = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
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
              (
                read__a
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_nothing := (
              (
                Atdgen_runtime.Oj_run.read_null
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
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
              invalid_arg "out-of-bounds substring position or length";
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
                (
                  read__a
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_nothing := (
                (
                  Atdgen_runtime.Oj_run.read_null
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "data"; "nothing" |];
        (
          {
            data = !field_data;
            nothing = !field_nothing;
          }
         : 'a param)
      )
)
let param_of_string read__a s =
  read_param read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pair write__a write__b : _ -> ('a, 'b) pair -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"left\":";
    (
      write__a
    )
      ob x.left;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"right\":";
    (
      write__b
    )
      ob x.right;
    Bi_outbuf.add_char ob '}';
)
let string_of_pair write__a write__b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pair write__a write__b ob x;
  Bi_outbuf.contents ob
let read_pair read__a read__b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_left = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_right = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
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
              (
                read__a
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_right := (
              (
                read__b
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
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
              invalid_arg "out-of-bounds substring position or length";
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
                (
                  read__a
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_right := (
                (
                  read__b
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "left"; "right" |];
        (
          {
            left = !field_left;
            right = !field_right;
          }
         : ('a, 'b) pair)
      )
)
let pair_of_string read__a read__b s =
  read_pair read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 write__a write__b = (
  Atdgen_runtime.Oj_run.write_list (
    write_pair write__a write__a
  )
)
let string_of__1 write__a write__b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 write__a write__b ob x;
  Bi_outbuf.contents ob
let read__1 read__a read__b = (
  Atdgen_runtime.Oj_run.read_list (
    read_pair read__a read__a
  )
)
let _1_of_string read__a read__b s =
  read__1 read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pairs write__a = (
  write__1 write__a write__a
)
let string_of_pairs write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pairs write__a ob x;
  Bi_outbuf.contents ob
let read_pairs read__a = (
  read__1 read__a read__a
)
let pairs_of_string read__a s =
  read_pairs read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_label = (
  Yojson.Safe.write_string
)
let string_of_label ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_label ob x;
  Bi_outbuf.contents ob
let read_label = (
  Atdgen_runtime.Oj_run.read_string
)
let label_of_string s =
  read_label (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_labeled : _ -> labeled -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"flag\":";
    (
      write_valid
    )
      ob x.flag;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"lb\":";
    (
      write_label
    )
      ob x.lb;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.count;
    Bi_outbuf.add_char ob '}';
)
let string_of_labeled ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_labeled ob x;
  Bi_outbuf.contents ob
let read_labeled = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_flag = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_lb = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_count = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
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
              (
                read_valid
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_lb := (
              (
                read_label
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_count := (
              (
                Atdgen_runtime.Oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
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
              invalid_arg "out-of-bounds substring position or length";
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
                (
                  read_valid
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_lb := (
                (
                  read_label
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_count := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "flag"; "lb"; "count" |];
        (
          {
            flag = !field_flag;
            lb = !field_lb;
            count = !field_count;
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
  let ob = Bi_outbuf.create len in
  write_from_module_a ob x;
  Bi_outbuf.contents ob
let read_from_module_a = (
  A_j.read_from_module_a
)
let from_module_a_of_string s =
  read_from_module_a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
