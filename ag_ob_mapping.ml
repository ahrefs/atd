
open Printf
open Atd_ast
open Ag_error
open Ag_mapping

type o = Ag_ocaml.atd_ocaml_repr
type b = Ag_biniou.biniou_repr

type ob_mapping =
    (Ag_ocaml.atd_ocaml_repr, Ag_biniou.biniou_repr) Ag_mapping.mapping

type ob_def = 
    (Ag_ocaml.atd_ocaml_repr, Ag_biniou.biniou_repr) Ag_mapping.def


(*
  Translation of the types into the ocaml/biniou mapping.
*)

let rec mapping_of_expr (x : type_expr) : ob_mapping =
  match x with
      `Sum (loc, l, an) -> 
	let ocaml_t = `Sum (Ag_ocaml.get_ocaml_sum an) in
	let biniou_t = `Sum in
	`Sum (loc, Array.of_list (List.map mapping_of_variant l),
	      ocaml_t, biniou_t)

    | `Record (loc, l, an) ->
	let ocaml_t = `Record (Ag_ocaml.get_ocaml_record an) in
	let ocaml_field_prefix = Ag_ocaml.get_ocaml_field_prefix an in
	let biniou_t = `Record in
	`Record (loc,
                 Array.of_list
                   (List.map (mapping_of_field ocaml_field_prefix) l),
		 ocaml_t, biniou_t)

    | `Tuple (loc, l, an) ->
	let ocaml_t = `Tuple in
	let biniou_t = `Tuple in
	`Tuple (loc, Array.of_list (List.map mapping_of_cell l),
		ocaml_t, biniou_t)
	
    | `List (loc, x, an) ->
	let ocaml_t = `List (Ag_ocaml.get_ocaml_list an) in
	let biniou_t = `List (Ag_biniou.get_biniou_list an) in
	`List (loc, mapping_of_expr x, ocaml_t, biniou_t)

    | `Option (loc, x, an) ->
	let ocaml_t = `Option in
	let biniou_t = `Option in
	`Option (loc, mapping_of_expr x, ocaml_t, biniou_t)

    | `Shared (loc, x, a) ->
        let ocaml_t = `Shared (Ag_ocaml.get_ocaml_shared a) in
        let biniou_t = `Shared in
        let id = Atd_annot.get_field (fun s -> Some s) "" ["share"] "id" a in
        if id = "" then
          error loc "bug: missing or empty share.id annotation";
        `Shared (loc, id, 
                 mapping_of_expr x, ocaml_t, biniou_t)

    | `Name (loc, (loc2, s, l), an) ->
	(match s with
             "unit" ->
               `Unit (loc, `Unit, `Unit)
	   | "bool" ->
	       `Bool (loc, `Bool, `Bool)
	   | "int" ->
	       let o = Ag_ocaml.get_ocaml_int an in
	       let b = Ag_biniou.get_biniou_int an in
	       `Int (loc, `Int o, `Int b)
	   | "float" ->
	       `Float (loc, `Float, `Float)
	   | "string" -> 
	       `String (loc, `String, `String)
	   | s ->
	       `Name (loc, s, List.map mapping_of_expr l, None, None)
	)
    | `Tvar (loc, s) ->
	`Tvar (loc, s)

and mapping_of_cell (loc, x, an) =
  let default = Ag_ocaml.get_ocaml_default an in
  let doc = Ag_doc.get_doc loc an in
  let ocaml_t =
    `Cell {
      Ag_ocaml.ocaml_default = default;
      ocaml_fname = "";
      ocaml_mutable = false;
      ocaml_fdoc = doc;
    }
  in
  let biniou_t = `Cell in
  {
    cel_loc = loc;
    cel_value = mapping_of_expr x;
    cel_arepr = ocaml_t;
    cel_brepr = biniou_t
  }


and mapping_of_variant = function
    `Variant (loc, (s, an), o) ->
      let ocaml_cons = Ag_ocaml.get_ocaml_cons s an in
      let doc = Ag_doc.get_doc loc an in
      let ocaml_t =
	`Variant {
	  Ag_ocaml.ocaml_cons = ocaml_cons;
          ocaml_vdoc = doc;
	}
      in
      let biniou_t = `Variant in
      let arg =
	match o with
	    None -> None
	  | Some x -> Some (mapping_of_expr x) in
      {
	var_loc = loc;
	var_cons = s;
	var_arg = arg;
	var_arepr = ocaml_t;
	var_brepr = biniou_t
      }

  | `Inherit _ -> assert false 
      
and mapping_of_field ocaml_field_prefix = function
    `Field (loc, (s, fk, an), x) ->
      let fvalue = mapping_of_expr x in
      let ocaml_default, biniou_unwrapped =
	match fk, Ag_ocaml.get_ocaml_default an with
	    `Required, None -> None, false
	  | `Optional, None -> Some "None", true
	  | (`Required | `Optional), Some _ ->
	      error loc "Superfluous default OCaml value"
	  | `With_default, Some s -> Some s, false
	  | `With_default, None ->
	      (* will try to determine implicit default value later *)
	      None, false
      in
      let ocaml_fname = Ag_ocaml.get_ocaml_fname (ocaml_field_prefix ^ s) an in
      let ocaml_mutable = Ag_ocaml.get_ocaml_mutable an in
      let doc = Ag_doc.get_doc loc an in
      { f_loc = loc;
	f_name = s;
	f_kind = fk;
	f_value = fvalue;

	f_arepr = `Field {
	  Ag_ocaml.ocaml_default = ocaml_default;
	  ocaml_fname = ocaml_fname;
	  ocaml_mutable = ocaml_mutable;
          ocaml_fdoc = doc;
	};

	f_brepr = `Field { Ag_biniou.biniou_unwrapped = biniou_unwrapped };
      }

  | `Inherit _ -> assert false


let def_of_atd (loc, (name, param, an), x) =
  let ocaml_predef = Ag_ocaml.get_ocaml_predef `Biniou an in
  let doc = Ag_doc.get_doc loc an in
  let o =
    match as_abstract x with
        Some (loc2, an2) ->
          (match Ag_ocaml.get_ocaml_module_and_t `Biniou name an with
               None -> None
             | Some (types_module, main_module, ext_name) ->
                 let args = List.map (fun s -> `Tvar (loc, s)) param in
                 Some (`External
                         (loc, name, args, 
                          `External (types_module, main_module, ext_name),
                          `External)
                      )
          )
      | None -> Some (mapping_of_expr x)
  in
  {
    def_loc = loc;
    def_name = name;
    def_param = param;
    def_value = o;
    def_arepr = `Def { Ag_ocaml.ocaml_predef = ocaml_predef;
                       ocaml_ddoc = doc; };
    def_brepr = `Def;
  }
    
let defs_of_atd_module l =
  List.map (function `Type def -> def_of_atd def) l

let defs_of_atd_modules l =
  List.map (fun (is_rec, l) -> (is_rec, defs_of_atd_module l)) l
