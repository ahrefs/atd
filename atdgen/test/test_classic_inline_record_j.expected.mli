(* Auto-generated from "test_classic_inline_record.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type foo = Test_classic_inline_record_t.foo =  Foo of { x: int; y: float } 

val write_foo :
  Buffer.t -> foo -> unit
  (** Output a JSON value of type {!type:foo}. *)

val string_of_foo :
  ?len:int -> foo -> string
  (** Serialize a value of type {!type:foo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_foo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> foo
  (** Input JSON data of type {!type:foo}. *)

val foo_of_string :
  string -> foo
  (** Deserialize JSON data of type {!type:foo}. *)

