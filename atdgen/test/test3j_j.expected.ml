(* Auto-generated from "test3j.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type unixtime_list = Test3j_t.unixtime_list

type tf_variant = Test3j_t.tf_variant

type tf_record = Test3j_t.tf_record = { the_value: tf_variant; etc: string }

type json = Yojson.Safe.json

type dyn = Yojson.Safe.json

type t = Test3j_t.t = { foo: int; bar: json; baz: dyn }

type sf_adapted = Test3j_t.sf_adapted

type patch = Test3j_t.patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = Test3j_t.b = { thing: int }

type a = Test3j_t.a = { thing: string; other_thing: bool }

type adapted = Test3j_t.adapted

let write__1 = (
  Atdgen_runtime.Oj_run.write_list (
    Atdgen_runtime.Oj_run.write_float_as_int
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_unixtime_list = (
  write__1
)
let string_of_unixtime_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_unixtime_list ob x;
  Bi_outbuf.contents ob
let read_unixtime_list = (
  read__1
)
let unixtime_list_of_string s =
  read_unixtime_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_variant = (
  fun ob x ->
    match x with
      | `A x ->
        Bi_outbuf.add_string ob "[\"a\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `B x ->
        Bi_outbuf.add_string ob "[\"b\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_tf_variant ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_variant ob x;
  Bi_outbuf.contents ob
let read_tf_variant = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 1 then (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        0
                      )
                    | 'b' -> (
                        1
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `A x
            | 1 ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `B x
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 1 then (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        0
                      )
                    | 'b' -> (
                        1
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `A x
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `B x
            | _ -> (
                assert false
              )
        )
)
let tf_variant_of_string s =
  read_tf_variant (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_record : _ -> tf_record -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Test_lib.Tag_field_example.restore (
    fun ob x ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"the_value\":";
      (
        write_tf_variant
      )
        ob x.the_value;
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"etc\":";
      (
        Yojson.Safe.write_string
      )
        ob x.etc;
      Bi_outbuf.add_char ob '}';
  )
)
let string_of_tf_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_record ob x;
  Bi_outbuf.contents ob
let read_tf_record = (
  Atdgen_runtime.Oj_run.read_with_adapter Test_lib.Tag_field_example.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_the_value = ref (Obj.magic (Sys.opaque_identity 0.0)) in
      let field_etc = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
              | 3 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' then (
                    0
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
              field_the_value := (
                (
                  read_tf_variant
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_etc := (
                (
                  Atdgen_runtime.Oj_run.read_string
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
                | 3 -> (
                    if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 9 -> (
                    if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' then (
                      0
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
                field_the_value := (
                  (
                    read_tf_variant
                  ) p lb
                );
                bits0 := !bits0 lor 0x1;
              | 1 ->
                field_etc := (
                  (
                    Atdgen_runtime.Oj_run.read_string
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
          if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "the_value"; "etc" |];
          (
            {
              the_value = !field_the_value;
              etc = !field_etc;
            }
           : tf_record)
        )
  )
)
let tf_record_of_string s =
  read_tf_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_json = (
  Yojson.Safe.write_json
)
let string_of_json ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_json ob x;
  Bi_outbuf.contents ob
let read_json = (
  Yojson.Safe.read_json
)
let json_of_string s =
  read_json (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_dyn = (
  Yojson.Safe.write_json
)
let string_of_dyn ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_dyn ob x;
  Bi_outbuf.contents ob
let read_dyn = (
  Yojson.Safe.read_json
)
let dyn_of_string s =
  read_dyn (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_t : _ -> t -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"foo\":";
    (
      Yojson.Safe.write_int
    )
      ob x.foo;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bar\":";
    (
      write_json
    )
      ob x.bar;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"baz\":";
    (
      write_dyn
    )
      ob x.baz;
    Bi_outbuf.add_char ob '}';
)
let string_of_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t ob x;
  Bi_outbuf.contents ob
let read_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_foo = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_bar = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_baz = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 3 then (
            match String.unsafe_get s pos with
              | 'b' -> (
                  if String.unsafe_get s (pos+1) = 'a' then (
                    match String.unsafe_get s (pos+2) with
                      | 'r' -> (
                          1
                        )
                      | 'z' -> (
                          2
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 'f' -> (
                  if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' then (
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
              (
                Atdgen_runtime.Oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_bar := (
              (
                read_json
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_baz := (
              (
                read_dyn
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
            if len = 3 then (
              match String.unsafe_get s pos with
                | 'b' -> (
                    if String.unsafe_get s (pos+1) = 'a' then (
                      match String.unsafe_get s (pos+2) with
                        | 'r' -> (
                            1
                          )
                        | 'z' -> (
                            2
                          )
                        | _ -> (
                            -1
                          )
                    )
                    else (
                      -1
                    )
                  )
                | 'f' -> (
                    if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' then (
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
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_bar := (
                (
                  read_json
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_baz := (
                (
                  read_dyn
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
        if !bits0 <> 0x7 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "foo"; "bar"; "baz" |];
        (
          {
            foo = !field_foo;
            bar = !field_bar;
            baz = !field_baz;
          }
         : t)
      )
)
let t_of_string s =
  read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_sf_adapted = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_runtime.Util.Json.Adapter.One_field.restore (
    fun ob x ->
      match x with
        | `A x ->
          Bi_outbuf.add_string ob "[\"a\",";
          (
            Yojson.Safe.write_bool
          ) ob x;
          Bi_outbuf.add_char ob ']'
        | `B x ->
          Bi_outbuf.add_string ob "[\"b\",";
          (
            Yojson.Safe.write_int
          ) ob x;
          Bi_outbuf.add_char ob ']'
  )
)
let string_of_sf_adapted ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_sf_adapted ob x;
  Bi_outbuf.contents ob
let read_sf_adapted = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_runtime.Util.Json.Adapter.One_field.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            Yojson.Safe.read_space p lb;
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                try
                  if len = 1 then (
                    match String.unsafe_get s pos with
                      | 'a' -> (
                          0
                        )
                      | 'b' -> (
                          1
                        )
                      | _ -> (
                          raise (Exit)
                        )
                  )
                  else (
                    raise (Exit)
                  )
                with Exit -> (
                    Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                  )
            in
            let i = Yojson.Safe.map_ident p f lb in
            match i with
              | 0 ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `A x
              | 1 ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `B x
              | _ -> (
                  assert false
                )
          )
        | `Double_quote -> (
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
            in
            let i = Yojson.Safe.map_string p f lb in
            match i with
              | _ -> (
                  assert false
                )
          )
        | `Square_bracket -> (
            Yojson.Safe.read_space p lb;
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                try
                  if len = 1 then (
                    match String.unsafe_get s pos with
                      | 'a' -> (
                          0
                        )
                      | 'b' -> (
                          1
                        )
                      | _ -> (
                          raise (Exit)
                        )
                  )
                  else (
                    raise (Exit)
                  )
                with Exit -> (
                    Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                  )
            in
            let i = Yojson.Safe.map_ident p f lb in
            match i with
              | 0 ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `A x
              | 1 ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `B x
              | _ -> (
                  assert false
                )
          )
  )
)
let sf_adapted_of_string s =
  read_sf_adapted (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Atdgen_runtime.Oj_run.write_nullable (
    Yojson.Safe.write_int
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      Atdgen_runtime.Oj_run.read_int
    ) p lb) : _ option)
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Atdgen_runtime.Oj_run.write_std_option (
    write__2
  )
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
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_patch : _ -> patch -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    (match x.patch1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch1\":";
      (
        write__2
      )
        ob x;
    );
    (match x.patch2 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch2\":";
      (
        write__2
      )
        ob x;
    );
    (match x.patch3 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch3\":";
      (
        write__2
      )
        ob x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_patch ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_patch ob x;
  Bi_outbuf.contents ob
let read_patch = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_patch1 = ref (None) in
    let field_patch2 = ref (None) in
    let field_patch3 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 6 && String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' then (
            match String.unsafe_get s (pos+5) with
              | '1' -> (
                  0
                )
              | '2' -> (
                  1
                )
              | '3' -> (
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
            field_patch1 := (
              Some (
                (
                  read__2
                ) p lb
              )
            );
          | 1 ->
            field_patch2 := (
              Some (
                (
                  read__2
                ) p lb
              )
            );
          | 2 ->
            field_patch3 := (
              Some (
                (
                  read__2
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
              invalid_arg "out-of-bounds substring position or length";
            if len = 6 && String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' then (
              match String.unsafe_get s (pos+5) with
                | '1' -> (
                    0
                  )
                | '2' -> (
                    1
                  )
                | '3' -> (
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
              field_patch1 := (
                Some (
                  (
                    read__2
                  ) p lb
                )
              );
            | 1 ->
              field_patch2 := (
                Some (
                  (
                    read__2
                  ) p lb
                )
              );
            | 2 ->
              field_patch3 := (
                Some (
                  (
                    read__2
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
            patch1 = !field_patch1;
            patch2 = !field_patch2;
            patch3 = !field_patch3;
          }
         : patch)
      )
)
let patch_of_string s =
  read_patch (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_b : _ -> b -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_int
    )
      ob x.thing;
    Bi_outbuf.add_char ob '}';
)
let string_of_b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_b ob x;
  Bi_outbuf.contents ob
let read_b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
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
              (
                Atdgen_runtime.Oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
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
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "thing" |];
        (
          {
            thing = !field_thing;
          }
         : b)
      )
)
let b_of_string s =
  read_b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_a : _ -> a -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_string
    )
      ob x.thing;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"other_thing\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.other_thing;
    Bi_outbuf.add_char ob '}';
)
let string_of_a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_a ob x;
  Bi_outbuf.contents ob
let read_a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_other_thing = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
              (
                Atdgen_runtime.Oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_other_thing := (
              (
                Atdgen_runtime.Oj_run.read_bool
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
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_other_thing := (
                (
                  Atdgen_runtime.Oj_run.read_bool
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
        if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "thing"; "other_thing" |];
        (
          {
            thing = !field_thing;
            other_thing = !field_other_thing;
          }
         : a)
      )
)
let a_of_string s =
  read_a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_adapted = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_runtime.Util.Json.Adapter.Type_field.Default.restore (
    fun ob x ->
      match x with
        | `A x ->
          Bi_outbuf.add_string ob "[\"a\",";
          (
            write_a
          ) ob x;
          Bi_outbuf.add_char ob ']'
        | `B x ->
          Bi_outbuf.add_string ob "[\"b\",";
          (
            write_b
          ) ob x;
          Bi_outbuf.add_char ob ']'
  )
)
let string_of_adapted ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_adapted ob x;
  Bi_outbuf.contents ob
let read_adapted = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_runtime.Util.Json.Adapter.Type_field.Default.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            Yojson.Safe.read_space p lb;
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                try
                  if len = 1 then (
                    match String.unsafe_get s pos with
                      | 'a' -> (
                          0
                        )
                      | 'b' -> (
                          1
                        )
                      | _ -> (
                          raise (Exit)
                        )
                  )
                  else (
                    raise (Exit)
                  )
                with Exit -> (
                    Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                  )
            in
            let i = Yojson.Safe.map_ident p f lb in
            match i with
              | 0 ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_a
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `A x
              | 1 ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_b
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `B x
              | _ -> (
                  assert false
                )
          )
        | `Double_quote -> (
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
            in
            let i = Yojson.Safe.map_string p f lb in
            match i with
              | _ -> (
                  assert false
                )
          )
        | `Square_bracket -> (
            Yojson.Safe.read_space p lb;
            let f =
              fun s pos len ->
                if pos < 0 || len < 0 || pos + len > String.length s then
                  invalid_arg "out-of-bounds substring position or length";
                try
                  if len = 1 then (
                    match String.unsafe_get s pos with
                      | 'a' -> (
                          0
                        )
                      | 'b' -> (
                          1
                        )
                      | _ -> (
                          raise (Exit)
                        )
                  )
                  else (
                    raise (Exit)
                  )
                with Exit -> (
                    Atdgen_runtime.Oj_run.invalid_variant_tag p (String.sub s pos len)
                  )
            in
            let i = Yojson.Safe.map_ident p f lb in
            match i with
              | 0 ->
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
              | 1 ->
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
              | _ -> (
                  assert false
                )
          )
  )
)
let adapted_of_string s =
  read_adapted (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
