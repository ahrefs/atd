(* Auto-generated from "test_ambiguous_variant.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type ambiguous' = Test_ambiguous_variant_t.ambiguous' = 
    Int of int
  | String of string


type ambiguous = Test_ambiguous_variant_t.ambiguous = 
    Int of int
  | String of string


let write_ambiguous' : _ -> ambiguous' -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Json_adapters.Identity.restore (
    fun ob (x : ambiguous') ->
      match x with
        | Int x ->
          Buffer.add_string ob "[\"Int\",";
          (
            Yojson.Safe.write_int
          ) ob x;
          Buffer.add_char ob ']'
        | String x ->
          Buffer.add_string ob "[\"String\",";
          (
            Yojson.Safe.write_string
          ) ob x;
          Buffer.add_char ob ']'
  )
)
let string_of_ambiguous' ?(len = 1024) x =
  let ob = Buffer.create len in
  write_ambiguous' ob x;
  Buffer.contents ob
let read_ambiguous' = (
  Atdgen_runtime.Oj_run.read_with_adapter Json_adapters.Identity.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Atdgen_runtime.Yojson_extra.start_any_variant p lb with
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "Int" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Int x : ambiguous')
              | "String" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (String x : ambiguous')
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let ambiguous'_of_string s =
  read_ambiguous' (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ambiguous : _ -> ambiguous -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Json_adapters.Identity.restore (
    fun ob (x : ambiguous) ->
      match x with
        | Int x ->
          Buffer.add_string ob "[\"Int\",";
          (
            Yojson.Safe.write_int
          ) ob x;
          Buffer.add_char ob ']'
        | String x ->
          Buffer.add_string ob "[\"String\",";
          (
            Yojson.Safe.write_string
          ) ob x;
          Buffer.add_char ob ']'
  )
)
let string_of_ambiguous ?(len = 1024) x =
  let ob = Buffer.create len in
  write_ambiguous ob x;
  Buffer.contents ob
let read_ambiguous = (
  Atdgen_runtime.Oj_run.read_with_adapter Json_adapters.Identity.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Atdgen_runtime.Yojson_extra.start_any_variant p lb with
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "Int" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Int x : ambiguous)
              | "String" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (String x : ambiguous)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let ambiguous_of_string s =
  read_ambiguous (Yojson.Safe.init_lexer ()) (Lexing.from_string s)


(** {3 Generic Modules } *)
module Ambiguous' = struct
type nonrec t = ambiguous'
let write = write_ambiguous'
let read = read_ambiguous'
let to_string = string_of_ambiguous'
let of_string = ambiguous'_of_string
end
module Ambiguous = struct
type nonrec t = ambiguous
let write = write_ambiguous
let read = read_ambiguous
let to_string = string_of_ambiguous
let of_string = ambiguous_of_string
end
