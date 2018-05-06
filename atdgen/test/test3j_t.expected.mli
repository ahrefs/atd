(* Auto-generated from "test3j.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type unixtime_list = float list

type tf_variant = [ `A of int | `B of int ]

type tf_record = { the_value: tf_variant; etc: string }

type json = Yojson.Safe.json

type dyn = Yojson.Safe.json

type t = { foo: int; bar: json; baz: dyn }

type sf_adapted = [ `A of bool | `B of int ]

type patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = { thing: int }

type a = { thing: string; other_thing: bool }

type adapted = [ `A of a | `B of b ]
