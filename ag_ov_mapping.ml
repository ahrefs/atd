open Printf
open Atd_ast
open Ag_error
open Ag_mapping

type o = Ag_ocaml.atd_ocaml_repr
type v = Ag_validate.validate_repr

type ov_mapping =
    (Ag_ocaml.atd_ocaml_repr, Ag_validate.validate_repr) Ag_mapping.mapping

type ob_def = 
    (Ag_ocaml.atd_ocaml_repr, Ag_validate.validate_repr) Ag_mapping.def

(*
  Translation of the types into the ocaml/validate mapping.
*)

let rec mapping_of_expr (x : type_expr) : ov_mapping =
  let v = Ag_validate.get_validator in
  match x with
      `Sum (loc, l, an) -> 
	let ocaml_t = `Sum (Ag_ocaml.get_ocaml_sum an) in
	`Sum (loc, Array.of_list (List.map mapping_of_variant l),
	      ocaml_t, v an)

    | `Record (loc, l, an) ->
	let ocaml_t = `Record (Ag_ocaml.get_ocaml_record an) in
	let ocaml_field_prefix = Ag_ocaml.get_ocaml_field_prefix an in
	`Record (loc,
                 Array.of_list
                   (List.map (mapping_of_field ocaml_field_prefix) l),
		 ocaml_t, v an)

    | `Tuple (loc, l, an) ->
	let ocaml_t = `Tuple in
	`Tuple (loc, Array.of_list (List.map mapping_of_cell l),
		ocaml_t, v an)
	
    | `List (loc, x, an) ->
	let ocaml_t = `List (Ag_ocaml.get_ocaml_list an) in
	`List (loc, mapping_of_expr x, ocaml_t, v an)

    | `Option (loc, x, an) ->
	let ocaml_t = `Option in
	`Option (loc, mapping_of_expr x, ocaml_t, v an)

    | `Shared (loc, x, an) ->
        let ocaml_t = `Shared (Ag_ocaml.get_ocaml_shared an) in
        let id = Atd_annot.get_field (fun s -> Some s) "" ["share"] "id" an in
        if id = "" then
          error loc "bug: missing or empty share.id annotation";
        `Shared (loc, id, 
                 mapping_of_expr x, ocaml_t, v an)

    | `Name (loc, (loc2, s, l), an) ->
	(match s with
             "unit" ->
               `Unit (loc, `Unit, v an)
	   | "bool" ->
	       `Bool (loc, `Bool, v an)
	   | "int" ->
	       let o = Ag_ocaml.get_ocaml_int an in
	       `Int (loc, `Int o, v an)
	   | "float" ->
	       `Float (loc, `Float, v an)
	   | "string" -> 
	       `String (loc, `String, v an)
	   | s ->
               `Name (loc, s, List.map mapping_of_expr l)
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
  {
    cel_loc = loc;
    cel_value = mapping_of_expr x;
    cel_arepr = ocaml_t;
    cel_brepr = None
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
      let arg =
	match o with
	    None -> None
	  | Some x -> Some (mapping_of_expr x) in
      {
	var_loc = loc;
	var_cons = s;
	var_arg = arg;
	var_arepr = ocaml_t;
	var_brepr = None
      }

  | `Inherit _ -> assert false 
      
and mapping_of_field ocaml_field_prefix = function
    `Field (loc, (s, fk, an), x) ->
      let fvalue = mapping_of_expr x in
      let ocaml_default =
	match fk, Ag_ocaml.get_ocaml_default an with
	    `Required, None -> None
	  | `Optional, None -> Some "None"
	  | (`Required | `Optional), Some _ ->
	      error loc "Superfluous default OCaml value"
	  | `With_default, Some s -> Some s
	  | `With_default, None ->
	      (* will try to determine implicit default value later *)
	      None
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

	f_brepr = None;
      }

  | `Inherit _ -> assert false


let def_of_atd (loc, (name, param, an), x) =
  let ocaml_predef = Ag_ocaml.get_ocaml_predef `Default an in
  let doc = Ag_doc.get_doc loc an in
  let o =
    match as_abstract x with
        Some (loc2, an2) ->
          (match Ag_ocaml.get_ocaml_module_and_t `Default name an with
               None -> None
             | Some (module_path, ext_name) ->
                 let args = List.map (fun s -> `Tvar (loc, s)) param in
                 Some (`External
                         (loc, name, args, 
                          `External (module_path, ext_name), None)
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
    def_brepr = None;
  }
    
let defs_of_atd_module l =
  List.map (function `Type def -> def_of_atd def) l

let defs_of_atd_modules l =
  List.map (fun (is_rec, l) -> (is_rec, defs_of_atd_module l)) l
