(* Auto-generated from "test_annot_tags.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type t = Test_annot_tags_t.t = {
  id: int;
  always_array: string Atdgen_runtime.Util.ocaml_array;
  melange_array: string Atdgen_runtime.Util.ocaml_array;
  ml_array: string list
}

val write_t :
  Buffer.t -> t -> unit
  (** Output a JSON value of type {!type:t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!type:t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!type:t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!type:t}. *)

