(*
  Tools shared between OCaml code generators.
  (ox means OCaml-X)
*)

open Printf

open Atd_import
open Ag_error
open Ag_mapping

type 'a expr = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.mapping
type 'a def = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type name = (loc * loc * string)
    (* location of the containing record or variant,
       location of the field definition,
       field/constructor name *)

type names = {
  field_names : name list list;
  poly_variant_names : name list list;
  classic_variant_names : name list list;
}

let rec extract_names_from_expr ?(is_root = false) root_loc acc (x : 'a expr) =
  match x with
      `Unit _
    | `Bool _
    | `Int _
    | `Float  _
    | `String _ -> acc
    | `Sum (loc, va, o, _) ->
        let l, (fn, pvn, cvn) =
          Array.fold_left (extract_names_from_variant root_loc) ([], acc) va
        in
        (match o with
             `Sum x ->
               (match x with
                    `Poly -> (fn, l :: pvn, cvn)
                  | `Classic ->
                      if is_root then (fn, pvn, l :: cvn)
                      else
                        error loc
                          "Anonymous classic variant types are not allowed \
                           by OCaml."
               )
           | _ -> assert false
        )

    | `Record (loc, fa, _, _) ->
        if is_root then
          let l, (fn, pvn, cvn) =
            Array.fold_left (extract_names_from_field root_loc) ([], acc) fa
          in
          (l :: fn, pvn, cvn)
        else
          error loc "Anonymous record types are not allowed by OCaml."

    | `Tuple (_, ca, _, _) ->
        Array.fold_left (extract_names_from_cell root_loc) acc ca

    | `List (_, x, _, _)
    | `Option (_, x, _, _)
    | `Nullable (_, x, _, _)
    | `Wrap (_, x, _, _) ->
        extract_names_from_expr root_loc acc x

    | `Name (_, _, l, _, _) ->
        List.fold_left (extract_names_from_expr root_loc) acc l

    | `External (_, _, l, _, _) ->
        List.fold_left (extract_names_from_expr root_loc) acc l

    | `Tvar _ -> acc

and extract_names_from_variant root_loc (l, acc) x =
  let l =
    match x.var_arepr with
        `Variant v -> (root_loc, x.var_loc, v.Ag_ocaml.ocaml_cons) :: l
      | _ -> assert false
  in
  match x.var_arg with
      None -> (l, acc)
    | Some x ->
        (l, extract_names_from_expr root_loc acc x)

and extract_names_from_field root_loc (l, acc) x =
  let l =
    match x.f_arepr with
        `Field f -> (root_loc, x.f_loc, f.Ag_ocaml.ocaml_fname) :: l
      | _ -> assert false
  in
  (l, extract_names_from_expr root_loc acc x.f_value)

and extract_names_from_cell root_loc acc x =
  extract_names_from_expr root_loc acc x.cel_value


let extract_ocaml_names_from_defs l =
  let fn, pvn, cvn =
    List.fold_left (
      fun acc def ->
        match def.def_value with
            None -> acc
          | Some x ->
              let root_loc = loc_of_mapping x in
              extract_names_from_expr ~is_root:true root_loc acc x
    ) ([], [], []) l
  in
  {
    field_names = List.rev fn;
    poly_variant_names = List.rev pvn;
    classic_variant_names = List.rev cvn;
  }

let flatten_defs (grouped_defs : 'a grouped_defs) : 'a def list =
  List.flatten (List.map snd grouped_defs)


let check_duplicate_names container_kind field_kind l =
  let tbl = Hashtbl.create 200 in
  List.iter (
    fun (root_loc, loc, s) ->
      try
        let orig_loc = Hashtbl.find tbl s in
        let msg1 =
          sprintf "\
%s contains a %s that is already defined elsewhere
and cannot be reused."
            (String.capitalize_ascii container_kind) field_kind
        in
        let msg2 = sprintf "First definition of %s %s." field_kind s in
        let msg3 = sprintf "\
Impossible second definition of %s %s.

Use a different name, possibly by placing <ocaml name=\"NAME\">
after the field name or variant name in the ATD type definition.
<ocaml field_prefix=\"PREFIX\"> can also be used after a whole record."
          field_kind s
        in
        if loc <> orig_loc then
          error3
            root_loc msg1
            orig_loc msg2
            loc msg3
        else
          error2
            root_loc msg1
            orig_loc msg2

      with Not_found ->
        Hashtbl.add tbl s loc
  ) l

let check_names x =
  check_duplicate_names "record type" "field name"
    (List.flatten x.field_names);
  check_duplicate_names "variant type" "constructor name"
    (List.flatten x.classic_variant_names)


let check grouped_defs =
  let x = extract_ocaml_names_from_defs (flatten_defs grouped_defs) in
  check_names x


let get_full_type_name x =
  let s = x.def_name in
  match x.def_param with
      [] -> s
    | [x] -> sprintf "'%s %s" x s
    | l ->
        let l = List.map (fun s -> "'" ^ s) l in
        sprintf "(%s) %s" (String.concat ", " l) s

let anon_param_type_name s n_param =
  match n_param with
  | 0 -> s
  | 1 -> "_ " ^ s
  | n ->
      let underscores = Array.make n "_" in
      let params = String.concat ", " (Array.to_list underscores) in
      "(" ^ params ^ ") " ^ s

(* Get a type expression that uses the original user-given name (e.g. not _1) *)
let get_type_constraint ~original_types def =
  try
    let (poly_name, n_params) = Hashtbl.find original_types def.def_name in
    anon_param_type_name poly_name n_params
  with Not_found ->
    get_full_type_name def


(* Classic variants and records need type annotations in order to allow
   constructor/field name disambiguation *)
let needs_type_annot (x : _ expr) =
  match x with
  | `Record (_, _, `Record `Record, _)
  | `Sum (_, _, `Sum `Classic, _) -> true
  | _ -> false

let insert_annot type_annot =
  match type_annot with
  | None -> ""
  | Some t -> sprintf " : %s" t

(* Add an optional type annotation on an OCaml expression or pattern *)
let opt_annot type_annot expr =
  match type_annot with
  | None -> expr
  | Some t -> sprintf "(%s : %s)" expr t

(* Add an optional type annotation after all function parameters
   in a let binding (last thing before the equal sign) *)
let opt_annot_def type_annot fun_param =
  match type_annot with
  | None -> fun_param
  | Some t -> sprintf "%s : %s" fun_param t


let write_file file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let write_ocaml out mli ml =
  match out with
      `Stdout ->
        printf "\
struct
%s
end :
sig
%s
end
"
          ml mli;
        flush stdout

    | `Files prefix ->
        write_file (prefix ^ ".mli") mli;
        write_file (prefix ^ ".ml") ml

let is_exportable def =
  let s = def.def_name in
  s <> "" && s.[0] <> '_'
  && def.def_value <> None

let make_record_creator deref x =
  match x.def_value with
      Some (`Record (_, a, `Record `Record, _)) ->
        let s = x.def_name in
        let full_name = get_full_type_name x in
        let l =
          Array.to_list
            (Array.map (Ag_ocaml.map_record_creator_field deref) a) in
        let intf_params = List.map (fun (x, _, _) -> x) l in
        let intf =
          sprintf "\
val create_%s :%s
  unit -> %s
  (** Create a record of type {!%s}. *)

"
            s (String.concat "" intf_params)
            full_name
            s
        in
        let impl_params = List.map (fun (_, x, _) -> x) l in
        let impl_fields = List.map (fun (_, _, x) -> x) l in
        let impl =
          sprintf "\
let create_%s %s
  () : %s =
  {%s
  }
"
            s (String.concat "" impl_params) full_name
            (String.concat "" impl_fields)
        in
        intf, impl

    | _ -> "", ""

let rec is_function (l : Ag_indent.t list) =
  match l with
      [] -> false
    | x :: _ ->
        match x with
            `Line _ -> false
          | `Block l -> is_function l
          | `Inline l -> is_function l
          | `Annot ("fun", _) -> true
          | `Annot (_, x) -> is_function [x]
