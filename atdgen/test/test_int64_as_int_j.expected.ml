(* Auto-generated from "test_int64_as_int.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type i1 = Test_int64_as_int_t.i1

type i = Test_int64_as_int_t.i

let write_i1 = (
  Atdgen_runtime.Oj_run.write_int64_as_int
)
let string_of_i1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_i1 ob x;
  Bi_outbuf.contents ob
let read_i1 = (
  Atdgen_runtime.Oj_run.read_int64
)
let i1_of_string s =
  read_i1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_i = (
  Atdgen_runtime.Oj_run.write_int64
)
let string_of_i ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_i ob x;
  Bi_outbuf.contents ob
let read_i = (
  Atdgen_runtime.Oj_run.read_int64
)
let i_of_string s =
  read_i (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
