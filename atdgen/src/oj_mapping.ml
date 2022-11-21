(*
  Translation of the types into the ocaml/json mapping.
*)

open Atd.Stdlib_extra
open Mapping
module A = Atd.Ast
module Json = Atd.Json
module R = Ocaml_repr
module An = Ocaml_annot

type t = (R.t, Json.json_repr) Mapping.t
type variant_mapping = (R.t, Json.json_repr) Mapping.variant_mapping

let check_json_sum loc json_sum_param variants =
  if json_sum_param.Json.json_open_enum then (
    let variants_with_arg =
      List.filter (function {var_arg = Some _; _} -> true | _ -> false) variants
    in
    match variants_with_arg with
    | [] ->
        Error.error loc
          "Missing catch-all case of the form `| Other of string`, \
           required by <json open_enum>."
    | [x] -> (
        match x.var_arg with
        | None -> assert false
        | Some (String _) -> ()
        | Some mapping ->
            let loc = Mapping.loc_of_mapping mapping in
            Error.error loc
              "The argument of this variant must be of type string \
               as imposed by <json open_enum>."
      )
    | _ ->
        Error.error loc
          "Multiple variants have arguments, which doesn't make sense \
           when combined with <json open_enum>."
  )

let rec mapping_of_expr (env : R.env) (x : A.type_expr) =
  match x with
  | Sum (loc, l, an) ->
      let ocaml_t = R.Sum (An.get_ocaml_sum Json an) in
      let json_sum_param = Json.get_json_sum an in
      let json_t = Json.Sum (Json.get_json_sum an) in
      let variants = List.map (mapping_of_variant env) l in
      check_json_sum loc json_sum_param variants;
      Sum (loc, Array.of_list variants,
           ocaml_t, json_t)

  | Record (loc, l, an) ->
      let ocaml_t = R.Record (An.get_ocaml_record Json an) in
      let ocaml_field_prefix = An.get_ocaml_field_prefix Json an in
      let json_t = Json.Record (Json.get_json_record an) in
      Record (loc,
              Array.of_list
                (List.map (mapping_of_field env ocaml_field_prefix) l),
              ocaml_t, json_t)

  | Tuple (loc, l, _) ->
      let ocaml_t = R.Tuple in
      let json_t = Json.Tuple in
      Tuple (loc, Array.of_list (List.map (mapping_of_cell env) l),
             ocaml_t, json_t)

  | List (loc, x, an) ->
      let ocaml_t = R.List (An.get_ocaml_list Json an) in
      let json_t = Json.List (Json.get_json_list an) in
      List (loc, mapping_of_expr env x, ocaml_t, json_t)

  | Option (loc, x, _) ->
      let ocaml_t = R.Option in
      let json_t = Json.Option in
      Option (loc, mapping_of_expr env x, ocaml_t, json_t)

  | Nullable (loc, x, _) ->
      let ocaml_t = R.Nullable in
      let json_t = Json.Nullable in
      Nullable (loc, mapping_of_expr env x, ocaml_t, json_t)

  | Shared (loc, _, _) ->
      Error.error loc "Sharing is not supported by the JSON interface"

  | Wrap (loc, x, an) ->
      let ocaml_t =
        R.Wrap (An.get_ocaml_wrap ~type_param:[] Json loc an) in
      let json_t = Json.Wrap in
      Wrap (loc, mapping_of_expr env x, ocaml_t, json_t)

  | Name (loc, (loc2, s, l), an) ->
      (match s with
       | TN ["unit"] ->
           Unit (loc, Unit, Unit)
       | TN ["bool"] ->
           Bool (loc, Bool, Bool)
       | TN ["int"] ->
           let o = An.get_ocaml_int Json an in
           Int (loc, Int o, Int)
       | TN ["float"] ->
           let j = Json.get_json_float an in
           Float (loc, Float, Float j)
       | TN ["string"] ->
           String (loc, String, String)
       | TN ["abstract"] ->
           Abstract (loc, Abstract, Abstract)
       | name ->
           let name = R.name env loc2 name in
           Name (loc, s, List.map (mapping_of_expr env) l, Name name, Name)
      )
  | Tvar (loc, s) ->
      Tvar (loc, s)

and mapping_of_cell env (cel_loc, x, an) =
  {
    cel_loc;
    cel_value = mapping_of_expr env x;
    cel_arepr =
      R.Cell {
        ocaml_default = An.get_ocaml_default Json an;
        ocaml_fname = "";
        ocaml_mutable = false;
        ocaml_fdoc = Atd.Doc.get_doc cel_loc an;
      };
    cel_brepr = Json.Cell
  }

and mapping_of_variant env (x : A.variant) =
  match x with
  | Inherit _ -> assert false
  | Variant (var_loc, (var_cons, an), o) ->
      {
        var_loc;
        var_cons;
        var_arg = Option.map (mapping_of_expr env) o;
        var_arepr = Variant {
          ocaml_cons = An.get_ocaml_cons Json var_cons an;
          ocaml_vdoc = Atd.Doc.get_doc var_loc an
        };
        var_brepr =
          Json.Variant {
            Json.json_cons = Json.get_json_cons var_cons an
          }
      }

and mapping_of_field env ocaml_field_prefix (x : A.field) =
  match x with
  | Inherit _ -> assert false
  | Field (f_loc, (f_name, f_kind, an), x) ->
      let { Ox_mapping.ocaml_default; unwrapped } =
        Ox_mapping.analyze_field Json f_loc f_kind an in
      {
        f_loc;
        f_name;
        f_kind;
        f_value = mapping_of_expr env x;
        f_arepr = R.Field {
          ocaml_default;
          ocaml_fname =
            An.get_ocaml_fname Json (ocaml_field_prefix ^ f_name) an;
          ocaml_mutable = An.get_ocaml_mutable Json an;
          ocaml_fdoc = Atd.Doc.get_doc f_loc an;
        };
        f_brepr = Json.Field {
          Json.json_fname = Json.get_json_fname f_name an;
          json_unwrapped = unwrapped;
        }
      }

let defs_of_def_groups def_groups (env : R.env) =
  (match env.target with
   | Json
   | Bucklescript -> ()
   | t -> invalid_arg "target must be json or bucklescript");
  List.map (fun (is_rec, defs) ->
    (is_rec,
     List.map (function (def : A.type_def) ->
        Ox_emit.def_of_atd def
          ~env
          ~external_:Json.External
          ~mapping_of_expr:(mapping_of_expr env)
          ~def:Json.Def
      ) defs
    )
  ) def_groups
