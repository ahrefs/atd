(* $Id: ag_ox_emit.ml 52210 2010-11-22 09:50:55Z martin $ *)

(*
  Tools shared between OCaml code generators.
  (ox means OCaml-X)
*)

open Printf

open Ag_error
open Ag_mapping

type 'a expr = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.mapping
type 'a def = (Ag_ocaml.atd_ocaml_repr, 'a) Ag_mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type name = (loc * string)

type names = {
  field_names : name list list;
  poly_variant_names : name list list;
  classic_variant_names : name list list;
}

let rec extract_names_from_expr ?(is_root = false) acc (x : 'a expr) =
  match x with
      `Unit _
    | `Bool _
    | `Int _
    | `Float  _
    | `String _ -> acc
    | `Sum (loc, va, o, _) ->
        let l, (fn, pvn, cvn) =
          Array.fold_left extract_names_from_variant ([], acc) va
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
            Array.fold_left extract_names_from_field ([], acc) fa
          in
          (l :: fn, pvn, cvn)
        else
          error loc "Anonymous record types are not allowed by OCaml."

    | `Tuple (loc, ca, _, _) ->
        Array.fold_left extract_names_from_cell acc ca

    | `List (loc, x, _, _)
    | `Option (loc, x, _, _)
    | `Shared (loc, _, x, _, _) ->
        extract_names_from_expr acc x

    | `Name (loc, _, l, _, _) ->
        List.fold_left extract_names_from_expr acc l

    | `External (loc, _, l, _, _) ->
        List.fold_left extract_names_from_expr acc l

    | `Tvar _ -> acc

and extract_names_from_variant (l, acc) x =
  let l =
    match x.var_arepr with
        `Variant v -> (x.var_loc, v.Ag_ocaml.ocaml_cons) :: l
    | _ -> assert false
  in
  match x.var_arg with
      None -> (l, acc)
    | Some x ->
        (l, extract_names_from_expr acc x)

and extract_names_from_field (l, acc) x =
  let l =
    match x.f_arepr with
        `Field f -> (x.f_loc, f.Ag_ocaml.ocaml_fname) :: l
    | _ -> assert false
  in
  (l, extract_names_from_expr acc x.f_value)

and extract_names_from_cell acc x =
  extract_names_from_expr acc x.cel_value


let extract_ocaml_names_from_defs l =
  let fn, pvn, cvn =
    List.fold_left (
      fun acc def ->
        match def.def_value with
            None -> acc
          | Some x -> extract_names_from_expr ~is_root:true acc x
    ) ([], [], []) l
  in
  {
    field_names = List.rev fn;
    poly_variant_names = List.rev pvn;
    classic_variant_names = List.rev cvn;
  }

let flatten_defs (grouped_defs : 'a grouped_defs) : 'a def list =
  List.flatten (List.map snd grouped_defs)


let check_duplicate_names kind l =
  let tbl = Hashtbl.create 200 in
  List.iter (
    fun (loc, s) ->
      try
        let loc0 = Hashtbl.find tbl s in
        error2
          loc0 (sprintf "First definition of %s %s." kind s)
          loc (sprintf "\
Impossible second definition of %s %s.

Use a different name, possibly by placing <ocaml name=\"NAME\">
after the field name or variant name in the ATD type definition.
<ocaml field_prefix=\"PREFIX\"> can also be used after a whole record."
                 kind s)

      with Not_found ->
        Hashtbl.add tbl s loc
  ) l

let check_names x =
  check_duplicate_names "record field name"
    (List.flatten x.field_names);
  check_duplicate_names "classic variant name"
    (List.flatten x.classic_variant_names)


let check grouped_defs =
  let x = extract_ocaml_names_from_defs (flatten_defs grouped_defs) in
  check_names x

let write_file file s =
  let oc = open_out file in
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
