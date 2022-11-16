(*
  Table of predefined types.
*)

open Printf
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

type table = (string, int * Ast.type_def option) Hashtbl.t

let make_table user_defs : table =
  let predef = Hashtbl.create 20 in
  (* add predefined types *)
  List.iter (
    fun (k, n, opt_t) ->
      if Hashtbl.mem predef k then
        invalid_arg ("Predef.make_table: duplicate entry " ^ k)
      else
        Hashtbl.add predef k (n, opt_t)
  ) list;
  let tbl = Hashtbl.copy predef in
  (* add user definitions *)
  List.iter (
    fun (x : type_def) ->
      let name = x.name in
      let loc = x.loc in
      if Hashtbl.mem tbl name then
        if Hashtbl.mem predef name then
          error_at loc
            (sprintf "%s is a predefined type, it cannot be redefined." name)
        else
          error_at loc
            (sprintf "Type %s is defined for the second time." name)
      else
        Hashtbl.add tbl name (List.length x.param, Some x)
  ) user_defs;
  tbl

let rec get_original_definition tbl name =
  match Hashtbl.find_opt tbl name with
  | None -> None
  | Some (n, opt_def) as res ->
      match opt_def with
      | None -> res
      | Some def ->
          match def.value with
          | Name (loc, (loc2, name, args), an ) ->
              (match get_original_definition tbl name with
               | None -> res
               | Some _ as res -> res
              )
          | _ -> res

let get_construct tbl name =
  match get_original_definition tbl name with
  | None -> None
  | Some (_n, None) -> None
  | Some (n, Some def) -> Some (n, def.value)

let get_construct_of_expr tbl (x : type_expr) =
  match x with
  | Name (loc, (loc2, name, []), an) ->
      (match get_original_definition tbl name with
       | None -> None
       | Some (_n, None) -> Some x
       | Some (n, Some def) -> Some def.value
      )
  | construct -> Some construct
