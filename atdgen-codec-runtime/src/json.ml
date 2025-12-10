
type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of t list
  | `Null
  | `String of string ]

let constr0 s = `String s

let constr1 s a = `List [`String s; a]
