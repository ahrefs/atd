type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option ]

type 'a t

val decode : 'a t -> json -> 'a

val unit : unit t
val bool : bool t
val int : int t
val float : float t
val char : char t

val optional : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

(* a field that should be present *)
val field : string -> 'a t -> 'a t

(* a field that turns into a an optional value when absent *)
val fieldDefault : string -> 'a -> 'a t -> 'a t

(* a field that returns None when is absent *)
val fieldOptional : string -> 'a t -> 'a option t

val map : ('a -> 'b) -> 'a t -> 'b t
