open Atd.Stdlib_extra
open Atd.Ast
open Mapping
module An = Ocaml_annot
module R = Ocaml_repr

type ob_mapping =
  (Ocaml_repr.t, Biniou.biniou_repr) Mapping.t

(*
  Translation of the types into the ocaml/biniou mapping.
*)

let rec mapping_of_expr (x : type_expr) : ob_mapping =
  match x with
    Sum (loc, l, an) ->
      let ocaml_t : R.t = Sum (An.get_ocaml_sum Biniou an) in
      let biniou_t = Biniou.Sum in
      Sum (loc, Array.of_list (List.map mapping_of_variant l),
           ocaml_t, biniou_t)

  | Record (loc, l, an) ->
      let ocaml_t = R.Record (An.get_ocaml_record Biniou an) in
      let ocaml_field_prefix = An.get_ocaml_field_prefix Biniou an in
      let biniou_t = Biniou.Record in
      Record (loc,
              Array.of_list
                (List.map (mapping_of_field ocaml_field_prefix) l),
              ocaml_t, biniou_t)

  | Tuple (loc, l, _) ->
      let ocaml_t = R.Tuple in
      let biniou_t = Biniou.Tuple in
      Tuple (loc, Array.of_list (List.map mapping_of_cell l),
             ocaml_t, biniou_t)

  | List (loc, x, an) ->
      let ocaml_t = R.List (Ocaml.get_ocaml_list Biniou an) in
      let biniou_t = Biniou.List (Biniou.get_biniou_list an) in
      List (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Option (loc, x, _) ->
      let ocaml_t = R.Option in
      let biniou_t = Biniou.Option in
      Option (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Nullable (loc, x, _) ->
      let ocaml_t = R.Nullable in
      let biniou_t = Biniou.Nullable in
      Nullable (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Shared (_, _, _) ->
      failwith "Sharing is no longer supported"

  | Wrap (loc, x, a) ->
      let ocaml_t =
        R.Wrap (Ocaml.get_ocaml_wrap ~type_param:[] Biniou loc a) in
      let json_t = Biniou.Wrap in
      Wrap (loc, mapping_of_expr x, ocaml_t, json_t)

  | Name (loc, (_, name, l), an) ->
      (match name with
       | TN ["unit"] ->
           Unit (loc, Unit, Biniou.Unit)
       | TN ["bool"] ->
           Bool (loc, Bool, Biniou.Bool)
       | TN ["int"] ->
           let o = Ocaml.get_ocaml_int Biniou an in
           let b = Biniou.get_biniou_int an in
           Int (loc, Int o, Biniou.Int b)
       | TN ["float"] ->
           let b = Biniou.get_biniou_float an in
           Float (loc, Float, Biniou.Float b)
       | TN ["string"] ->
           String (loc, String, Biniou.String)
       | name ->
           Name (loc, name, List.map mapping_of_expr l, None, None)
      )
  | Tvar (loc, s) ->
      Tvar (loc, s)

and mapping_of_cell (cel_loc, x, an) =
  { cel_loc
  ; cel_value = mapping_of_expr x
  ; cel_arepr = R.Cell
        { Ocaml.ocaml_default = Ocaml.get_ocaml_default Biniou an
        ; ocaml_fname = ""
        ; ocaml_mutable = false
        ; ocaml_fdoc = Atd.Doc.get_doc cel_loc an
        }
  ; cel_brepr = Biniou.Cell
  }

and mapping_of_variant = function
  | Inherit _ -> assert false
  | Variant (var_loc, (var_cons, an), o) ->
      { var_loc
      ; var_cons
      ; var_arg = Option.map mapping_of_expr o
      ; var_arepr = R.Variant
            { Ocaml.ocaml_cons = Ocaml.get_ocaml_cons Biniou var_cons an
            ; ocaml_vdoc = Atd.Doc.get_doc var_loc an
            }
      ; var_brepr = Biniou.Variant
      }

and mapping_of_field ocaml_field_prefix = function
  | Inherit _ -> assert false
  | Field (f_loc, (f_name, f_kind, an), x) ->
      let { Ox_mapping.ocaml_default; unwrapped } =
        Ox_mapping.analyze_field Biniou f_loc f_kind an in
      { f_loc
      ; f_name
      ; f_kind
      ; f_value = mapping_of_expr x
      ; f_arepr = R.Field
            { Ocaml.ocaml_default
            ; ocaml_fname =
                Ocaml.get_ocaml_fname Biniou (ocaml_field_prefix ^ f_name) an
            ; ocaml_mutable = Ocaml.get_ocaml_mutable Biniou an
            ; ocaml_fdoc = Atd.Doc.get_doc f_loc an
            }
      ; f_brepr = Biniou.Field { Biniou.biniou_unwrapped = unwrapped };
      }



let def_of_atd atd =
  Ox_emit.def_of_atd atd ~target:Biniou ~external_:Biniou.External
    ~mapping_of_expr ~def:Biniou.Def

let defs_of_def_groups l =
  List.map (fun (is_rec, defs) ->
    let defs = List.map def_of_atd defs  in
    (is_rec, defs)
  ) l
