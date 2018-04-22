(* Auto-generated from "test3j.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type unixtime_list = float list

type json = Yojson.Safe.json

type dyn = Yojson.Safe.json

type t = { foo: int; bar: json; baz: dyn }

type integer_or_string = [ `Integer of int | `String of string ]

type simple = { data: integer_or_string }

type patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = { thing: int }

type a = { thing: string; other_thing: bool }

type adapted = [ `A of a | `B of b ]
