(** OCaml-WWW-form runtime library. *)

exception Error of string

type state

type acc = (string list * string) list

type 'a write = state -> acc -> 'a -> acc

val error : string -> _

val make_item : state -> state
val make_field : state -> string -> state

val write_bool : bool write
val write_list : 'a write -> 'a list write
val write_array : 'a write -> 'a array write
val write_float : float write
val write_float_as_int : float write
val write_float_prec : int -> float write
val write_assoc_list : 'a write -> 'b write -> ('a * 'b) list write
val write_assoc_array : 'a write -> 'b write -> ('a * 'b) array write
val write_null : unit write
val write_option : 'a write -> 'a option write
val write_nullable : 'a write -> 'a option write
val write_int : int write
val write_int8 : char write
val write_int32 : int32 write
val write_int64 : int64 write
val write_string : string write

val www_form_of_acc : 'a write -> 'a -> (string * string) list
