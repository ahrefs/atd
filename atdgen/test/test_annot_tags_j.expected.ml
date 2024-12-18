(* Auto-generated from "test_annot_tags.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type t = Test_annot_tags_t.t = {
  id: int;
  always_array: string Atdgen_runtime.Util.ocaml_array;
  melange_array: string Atdgen_runtime.Util.ocaml_array;
  ml_array: string list
}

let write__x_f5dfc61 = (
  Atdgen_runtime.Oj_run.write_array (
    Yojson.Safe.write_string
  )
)
let string_of__x_f5dfc61 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_f5dfc61 ob x;
  Buffer.contents ob
let read__x_f5dfc61 = (
  Atdgen_runtime.Oj_run.read_array (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _x_f5dfc61_of_string s =
  read__x_f5dfc61 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write_t : _ -> t -> _ = (
  fun ob (x : t) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"id\":";
    (
      Yojson.Safe.write_int
    )
      ob x.id;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"always_array\":";
    (
      write__x_20d39e2
    )
      ob x.always_array;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"melange_array\":";
    (
      write__x_f5dfc61
    )
      ob x.melange_array;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"ml_array\":";
    (
      write__string_list
    )
      ob x.ml_array;
    Buffer.add_char ob '}';
)
let string_of_t ?(len = 1024) x =
  let ob = Buffer.create len in
  write_t ob x;
  Buffer.contents ob
let read_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_id = ref (None) in
    let field_always_array = ref (None) in
    let field_melange_array = ref (None) in
    let field_ml_array = ref (None) in
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
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'y' then (
                  3
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'y' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'y' then (
                  1
                )
                else (
                  -1
                )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'y' then (
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
            field_id := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_always_array := (
              Some (
                (
                  read__x_20d39e2
                ) p lb
              )
            );
          | 2 ->
            field_melange_array := (
              Some (
                (
                  read__x_f5dfc61
                ) p lb
              )
            );
          | 3 ->
            field_ml_array := (
              Some (
                (
                  read__string_list
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
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'y' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'y' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'y' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'y' then (
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
              field_id := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_always_array := (
                Some (
                  (
                    read__x_20d39e2
                  ) p lb
                )
              );
            | 2 ->
              field_melange_array := (
                Some (
                  (
                    read__x_f5dfc61
                  ) p lb
                )
              );
            | 3 ->
              field_ml_array := (
                Some (
                  (
                    read__string_list
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
            id = (match !field_id with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "id");
            always_array = (match !field_always_array with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "always_array");
            melange_array = (match !field_melange_array with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "melange_array");
            ml_array = (match !field_ml_array with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ml_array");
          }
         : t)
      )
)
let t_of_string s =
  read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
