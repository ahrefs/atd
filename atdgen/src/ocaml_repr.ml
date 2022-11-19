(*
   The translation of ATD type annotations into a typed representation.
   This was meant to clarify what a given ATD type can map to in OCaml.
*)

type target = Default | Biniou | Json | Validate | Bucklescript

type env = {
  target: target;
  imports: Atd.Imports.t;
}

let all_targets = [ Default; Biniou; Json; Validate; Bucklescript ]

type pp_convs =
  | Camlp4 of string list
  | Ppx_deriving of string list
  | Ppx of string list

(* Type mapping from ATD to OCaml *)

type atd_ocaml_sum = Classic | Poly
type atd_ocaml_record = Record | Object

type atd_ocaml_int = Int | Char | Int32 | Int64 | Float
type atd_ocaml_list = List | Array

type atd_ocaml_wrap = {
  ocaml_wrap_t : string;
  ocaml_wrap : string;
  ocaml_unwrap : string;
}

type atd_ocaml_field = {
  ocaml_default : string option;
  ocaml_fname : string;
  ocaml_mutable : bool;
  ocaml_fdoc : Atd.Doc.doc option;
}

type atd_ocaml_variant = {
  ocaml_cons : string;
  ocaml_vdoc : Atd.Doc.doc option;
}

type atd_ocaml_def = {
  ocaml_predef : bool;
  ocaml_ddoc : Atd.Doc.doc option;
}

let tick = function
  | Poly -> "`"
  | Classic -> ""

let dot = function
  | Record -> "."
  | Object -> "#"

type name = {
  (* Foo_t: OCaml module providing types *)
  type_module_name: string option;
  (* Foo_j or other suffix: OCaml module providing conversion functions. *)
  main_module_name: string option;
  (* Simple type name without parameters *)
  type_name: string;
}

type t =
  | Unit
  | Bool
  | Int of atd_ocaml_int
  | Float
  | String
  | Abstract
  | Sum of atd_ocaml_sum
  | Record of atd_ocaml_record
  | Tuple
  | List of atd_ocaml_list
  | Option
  | Nullable
  | Wrap of atd_ocaml_wrap option
  | Name of name
  | External of name
  | Cell of atd_ocaml_field
  | Field of atd_ocaml_field
  | Variant of atd_ocaml_variant
  | Def of atd_ocaml_def

let ocaml_int_of_string s : atd_ocaml_int option =
  match s with
      "int" -> Some Int
    | "char" -> Some Char
    | "int32" -> Some Int32
    | "int64" -> Some Int64
    | "float" -> Some Float
    | _ -> None

let string_of_ocaml_int (x : atd_ocaml_int) =
  match x with
      Int -> "int"
    | Char -> "Char.t"
    | Int32 -> "Int32.t"
    | Int64 -> "Int64.t"
    | Float -> "float"

let ocaml_sum_of_string s : atd_ocaml_sum option =
  match s with
      "classic" -> Some Classic
    | "poly" -> Some Poly
    | _ -> None

let ocaml_record_of_string s : atd_ocaml_record option =
  match s with
      "record" -> Some Record
    | "object" -> Some Object
    | _ -> None

let ocaml_list_of_string s : atd_ocaml_list option =
  match s with
      "list" -> Some List
    | "array" -> Some Array
    | _ -> None

let string_of_ocaml_list (x : atd_ocaml_list) =
  match x with
      List -> "list"
    | Array -> "Atdgen_runtime.Util.ocaml_array"

let ocaml_module_suffix (target : target) =
  match target with
  | Default -> "_t"
  | Biniou -> "_b"
  | Json -> "_j"
  | Bucklescript -> "_bs"
  | Validate -> "_v"

let ocaml_module_name target atd_name =
  String.capitalize_ascii atd_name ^ ocaml_module_suffix target

let ocaml_type_module_name atd_name =
   ocaml_module_name Default atd_name
