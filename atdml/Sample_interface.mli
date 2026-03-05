(*
   Sample mli file to be generated from an ATD file by atdml.

   I don't think we should offer functions to read and write types
   from files or channels because Yojson offers these functions.

   'make' functions to create records with optional fields are useful
   and should be generated.
*)

type foo = {
  a: int;
  b: string list;
}

type ('a, 'b) bar = Bar of 'a * 'b

val make_foo : ?a:int -> ?b:string list -> unit -> foo

(* This naming scheme would conflict with ATD type names 'yojson' and 'json',
   as well as OCaml keywords. Such ATD type names are automatically renamed
   (e.g. 'yojson' -> 'yojson_') to avoid conflicts. *)
val foo_of_yojson : Yojson.Safe.t -> foo
val yojson_of_foo : foo -> Yojson.Safe.t

val foo_of_json : string -> foo
val json_of_foo : foo -> string

val bar_of_yojson :
  (Yojson.Safe.t -> 'a) ->
  (Yojson.Safe.t -> 'b) ->
  Yojson.Safe.t -> ('a, 'b) bar

val yojson_of_bar :
  ('a -> Yojson.Safe.t) ->
  ('b -> Yojson.Safe.t) ->
  ('a, 'b) bar -> Yojson.Safe.t

val bar_of_json :
  (Yojson.Safe.t -> 'a) ->
  (Yojson.Safe.t -> 'b) ->
  string -> ('a, 'b) bar

val json_of_bar :
  ('a -> Yojson.Safe.t) ->
  ('b -> Yojson.Safe.t) ->
  ('a, 'b) bar -> string

(*
   Foo has the submodule signature for a type without parameters
   with an extra 'make' function whose signature is unlikely to be shared
   by other submodules. This is why every submodule has its own
   slightly different signature.
 *)
module Foo : sig
  (* Optionally, the type can be decorated with ppx annotations such as
     [@@deriving eq, ord, show] *)
  type nonrec t = foo

  val make : ?a:int -> ?b:string list -> unit -> t

  (* Other kinds of JSON ASTs could be supported in addition
     to Yojson.Safe.t *)
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t

  (* We don't expose 'of_string' and 'to_string' functions to avoid
     ambiguities as to what format we serializing to or deserializing from.
     This also allows atdml to support other formats that JSON in the future.
  *)
  val of_json : string -> t
  val to_json : t -> string
end

(* This is the submodule for the parametrized type 'bar'. *)
module Bar : sig
  type nonrec ('a, 'b) t = ('a, 'b) bar

  val of_yojson :
    (Yojson.Safe.t -> 'a) ->
    (Yojson.Safe.t -> 'b) ->
    Yojson.Safe.t -> ('a, 'b) t

  val to_yojson :
    ('a -> Yojson.Safe.t) ->
    ('b -> Yojson.Safe.t) ->
    ('a, 'b) t -> Yojson.Safe.t

  val of_json :
    (Yojson.Safe.t -> 'a) ->
    (Yojson.Safe.t -> 'b) ->
    string -> ('a, 'b) t

  val to_json :
    ('a -> Yojson.Safe.t) ->
    ('b -> Yojson.Safe.t) ->
    ('a, 'b) t -> string
end
