(* Auto-generated from "test3j.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type unixtime_list = Test3j_t.unixtime_list

type json = Yojson.Safe.json

type dyn = Yojson.Safe.json

type t = Test3j_t.t = { foo: int; bar: json; baz: dyn }

type patch = Test3j_t.patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = Test3j_t.b = { thing: int }

type a = Test3j_t.a = { thing: string; other_thing: bool }

type adapted = Test3j_t.adapted

val write_unixtime_list :
  Bi_outbuf.t -> unixtime_list -> unit
  (** Output a JSON value of type {!unixtime_list}. *)

val string_of_unixtime_list :
  ?len:int -> unixtime_list -> string
  (** Serialize a value of type {!unixtime_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unixtime_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unixtime_list
  (** Input JSON data of type {!unixtime_list}. *)

val unixtime_list_of_string :
  string -> unixtime_list
  (** Deserialize JSON data of type {!unixtime_list}. *)

val write_json :
  Bi_outbuf.t -> json -> unit
  (** Output a JSON value of type {!json}. *)

val string_of_json :
  ?len:int -> json -> string
  (** Serialize a value of type {!json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
  (** Input JSON data of type {!json}. *)

val json_of_string :
  string -> json
  (** Deserialize JSON data of type {!json}. *)

val write_dyn :
  Bi_outbuf.t -> dyn -> unit
  (** Output a JSON value of type {!dyn}. *)

val string_of_dyn :
  ?len:int -> dyn -> string
  (** Serialize a value of type {!dyn}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dyn :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dyn
  (** Input JSON data of type {!dyn}. *)

val dyn_of_string :
  string -> dyn
  (** Deserialize JSON data of type {!dyn}. *)

val write_t :
  Bi_outbuf.t -> t -> unit
  (** Output a JSON value of type {!t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!t}. *)

val write_patch :
  Bi_outbuf.t -> patch -> unit
  (** Output a JSON value of type {!patch}. *)

val string_of_patch :
  ?len:int -> patch -> string
  (** Serialize a value of type {!patch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_patch :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> patch
  (** Input JSON data of type {!patch}. *)

val patch_of_string :
  string -> patch
  (** Deserialize JSON data of type {!patch}. *)

val write_b :
  Bi_outbuf.t -> b -> unit
  (** Output a JSON value of type {!b}. *)

val string_of_b :
  ?len:int -> b -> string
  (** Serialize a value of type {!b}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_b :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> b
  (** Input JSON data of type {!b}. *)

val b_of_string :
  string -> b
  (** Deserialize JSON data of type {!b}. *)

val write_a :
  Bi_outbuf.t -> a -> unit
  (** Output a JSON value of type {!a}. *)

val string_of_a :
  ?len:int -> a -> string
  (** Serialize a value of type {!a}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_a :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> a
  (** Input JSON data of type {!a}. *)

val a_of_string :
  string -> a
  (** Deserialize JSON data of type {!a}. *)

val write_adapted :
  Bi_outbuf.t -> adapted -> unit
  (** Output a JSON value of type {!adapted}. *)

val string_of_adapted :
  ?len:int -> adapted -> string
  (** Serialize a value of type {!adapted}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_adapted :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> adapted
  (** Input JSON data of type {!adapted}. *)

val adapted_of_string :
  string -> adapted
  (** Deserialize JSON data of type {!adapted}. *)

