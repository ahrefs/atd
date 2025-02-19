(* Auto-generated from "test_polymorphic_wrap.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type 'a t = 'a Array_wrap.t

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
let write__x_9bf0425 write__a = (
  fun ob x -> (
    let x = ( Array_wrap.unwrap ) x in (
      write__a_list write__a
    ) ob x)
)
let string_of__x_9bf0425 write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write__x_9bf0425 write__a ob x;
  Buffer.contents ob
let read__x_9bf0425 read__a = (
  fun p lb ->
    let x = (
      read__a_list read__a
    ) p lb in
    ( Array_wrap.wrap ) x
)
let _x_9bf0425_of_string read__a s =
  read__x_9bf0425 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_t write__a = (
  write__x_9bf0425 write__a
)
let string_of_t write__a ?(len = 1024) x =
  let ob = Buffer.create len in
  write_t write__a ob x;
  Buffer.contents ob
let read_t read__a = (
  read__x_9bf0425 read__a
)
let t_of_string read__a s =
  read_t read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)


(** {3 Generic Modules } *)
module T = struct
type nonrec ('a) t = ('a) t
let write = write_t
let read = read_t
let to_string = string_of_t
let of_string = t_of_string
end
