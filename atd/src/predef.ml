(*
  Table of predefined types.
*)

open Ast

let set_orig (x : type_def) = { x with orig = Some x }

let list_def : type_def =
  let loc = dummy_loc in
  set_orig {
    loc;
    name = "list";
    param = ["a"];
    annot = [];
    value = List (loc, Tvar (loc, "a"), []);
    orig = None;
  }

let option_def : type_def =
  let loc = dummy_loc in
  set_orig {
    loc;
    name = "option";
    param = ["a"];
    annot = [];
    value = Option (loc, Tvar (loc, "a"), []);
    orig = None;
  }

let nullable_def : type_def =
  let loc = dummy_loc in
  set_orig {
    loc;
    name = "nullable";
    param = ["a"];
    annot = [];
    value = Nullable (loc, Tvar (loc, "a"), []);
    orig = None;
  }

let shared_def : type_def =
  let loc = dummy_loc in
  set_orig {
    loc;
    name = "shared";
    param = ["a"];
    annot = [];
    value = Shared (loc, Tvar (loc, "a"), []);
    orig = None;
  }

let wrap_def : type_def =
  let loc = dummy_loc in
  set_orig {
    loc;
    name = "wrap";
    param = ["a"];
    annot = [];
    value = Wrap (loc, Tvar (loc, "a"), []);
    orig = None;
  }

let list = [
    "unit", 0, None;
    "bool", 0, None;
    "int", 0, None;
    "float", 0, None;
    "string", 0, None;
    "abstract", 0, None;
    "list", 1, Some list_def;
    "option", 1, Some option_def;
    "nullable", 1, Some nullable_def;
    "shared", 1, Some shared_def;
    "wrap", 1, Some wrap_def;
  ]

let make_table () =
  let tbl = Hashtbl.create 20 in
  List.iter (
    fun (k, n, opt_t) ->
      Hashtbl.add tbl k (n, opt_t)
  ) list;
  tbl
