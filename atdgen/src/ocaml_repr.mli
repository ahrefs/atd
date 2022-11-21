(*
   OCaml-side describing how ATD types are represented in OCaml.

   Due to options specified via ATD annotations, the same ATD type
   can have multiple representations of OCaml. The types here contain
   the OCaml representation after the ATD annotations have been processed.
*)

type target = Default | Biniou | Json | Validate | Bucklescript

val all_targets : target list

type env = {
  target: target;
  imports: Atd.Imports.t;
}

type pp_convs =
  | Camlp4 of string list
  | Ppx_deriving of string list
  | Ppx of string list

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

type name = {
  (* Foo_t: OCaml module providing types *)
  type_module_name: string option;
  (* Foo_j or other suffix: OCaml module providing conversion functions. *)
  main_module_name: string option;
  (* Simple type name without parameters e.g. 'bar' *)
  base_type_name: string;
  (* Full type name with module, without parameters  e.g. 'Foo_t.bar' *)
  full_type_name: string
}

(** OCaml-specific options that decorate each kind of ATD AST node. *)
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

val ocaml_int_of_string : string -> atd_ocaml_int option
val ocaml_sum_of_string : string -> atd_ocaml_sum option
val ocaml_record_of_string : string -> atd_ocaml_record option
val ocaml_list_of_string : string -> atd_ocaml_list option

(* "foo" -> "Foo_j" *)
val ocaml_module_name : target -> string -> string

(* "foo" -> "Foo_t" *)
val ocaml_type_module_name : string -> string

(** Convert an ATD type name possibly depending on an import into
    OCaml module and type names. *)
val name : env -> Atd.Ast.loc -> Atd.Ast.type_name -> name

val init_env : Atd.Ast.import list -> target -> env

val obj_unimplemented : Atd.Ast.loc -> atd_ocaml_record -> unit
