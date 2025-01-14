(* Auto-generated from "test_classic_inline_record.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type foo = Test_classic_inline_record_t.foo =  Foo of { x: int; y: float } 

let write_foo : _ -> foo -> _ = (
  fun ob (x : foo) ->
    match x with
      | Foo x ->
        Buffer.add_string ob "[\"Foo\",";
        begin
          Buffer.add_char ob '{';
          let is_first = ref true in
          if !is_first then
            is_first := false
          else
            Buffer.add_char ob ',';
            Buffer.add_string ob "\"x\":";
          (
            Yojson.Safe.write_int
          )
            ob x.x;
          if !is_first then
            is_first := false
          else
            Buffer.add_char ob ',';
            Buffer.add_string ob "\"y\":";
          (
            Yojson.Safe.write_std_float
          )
            ob x.y;
          Buffer.add_char ob '}';
        end (* ob x *);
        Buffer.add_char ob ']'
)
let string_of_foo ?(len = 1024) x =
  let ob = Buffer.create len in
  write_foo ob x;
  Buffer.contents ob
let read_foo = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Foo" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_lcurl p lb;
                let field_x = ref (None) in
                let field_y = ref (None) in
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
                        field_x := (
                          Some (
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          )
                        );
                      | 1 ->
                        field_y := (
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
                        if len = 1 then (
                          match String.unsafe_get s pos with
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
                          field_x := (
                            Some (
                              (
                                Atdgen_runtime.Oj_run.read_int
                              ) p lb
                            )
                          );
                        | 1 ->
                          field_y := (
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
                    (Foo
                      {
                        x = (match !field_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x");
                        y = (match !field_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "y");
                      }
                     : foo)
                  )
              ) in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              x
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
            | "Foo" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_lcurl p lb;
                let field_x = ref (None) in
                let field_y = ref (None) in
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
                        field_x := (
                          Some (
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          )
                        );
                      | 1 ->
                        field_y := (
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
                        if len = 1 then (
                          match String.unsafe_get s pos with
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
                          field_x := (
                            Some (
                              (
                                Atdgen_runtime.Oj_run.read_int
                              ) p lb
                            )
                          );
                        | 1 ->
                          field_y := (
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
                    (Foo
                      {
                        x = (match !field_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x");
                        y = (match !field_y with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "y");
                      }
                     : foo)
                  )
              ) in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let foo_of_string s =
  read_foo (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
