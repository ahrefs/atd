(*
   Translate ATD to JSON Schema (JSS)

   https://json-schema.org/draft/2020-12/json-schema-core.html
*)

open Printf
open Ast

type json = Yojson.Safe.t

type type_expr =
  | Ref of string
  | Null
  | Boolean
  | Integer
  | Number
  | String
  | Array of type_expr
  | Tuple of type_expr list (* a variation on 'array' *)
  | Object of object_
  | Union of type_expr list
  | Nullable of type_expr
  | Const of json

and object_ = {
  properties: property list;
  required: string list; (* list of the properties that are required *)
}

and property = string * type_expr

type def = {
  id: string;
  description: string option;
  type_expr: type_expr;
}

(* The root of a JSON Schema *)
type t = {
  root_id: string;
  root_description: string;
  schema: string;
  defs: def list;
}

let make_id root_id_uri type_name =
  root_id_uri ^ "/" ^ type_name

let trans_type_expr ~root_id_uri (x : Ast.type_expr) : type_expr =
  let rec trans_type_expr (x : Ast.type_expr) : type_expr =
    match x with
    | Sum (loc, vl, an) ->
        Union (List.map (fun x ->
          match (x : variant) with
          | Variant (loc, (name, an), opt_e) ->
              let json_name = Json.get_json_cons name an in
              (match opt_e with
               | None -> Const (`String json_name)
               | Some e ->
                   Tuple [
                     Const (`String json_name);
                     trans_type_expr e;
                   ]
              )
          | Inherit _ -> assert false
        ) vl)
    | Record (loc, fl, an) ->
        let fields =
          List.map (fun (x : field) ->
            match x with
            | `Field ((loc, (name, kind, an), e) : simple_field) ->
                let json_name = Json.get_json_fname name an in
                let required =
                  match kind with
                  | Required -> Some json_name
                  | Optional
                  | With_default -> None
                in
                let unwrapped_e =
                  match kind, e with
                  | Optional, Option (loc, e, an) -> e
                  | _, e -> e
                in
                ((json_name, trans_type_expr unwrapped_e), required)
            | `Inherit _ -> assert false
          ) fl
        in
        let properties = List.map fst fields in
        let required =
          List.filter_map (fun (_, required) -> required) fields
        in
        Object { properties; required }
    | Tuple (loc, tl, an) ->
        Tuple (List.map (fun (loc, e, an) -> trans_type_expr e) tl)
    | List (loc, e, an) ->
        (* TODO: handle <json repr="object"> *)
        Array (trans_type_expr e)
    | Option (loc, e, an) ->
        (* usually not what the user intended *)
        let transpiled = Sum (loc, [
          Variant (loc, ("Some", []), Some e);
          Variant (loc, ("None", []), None);
        ], an)
        in
        trans_type_expr transpiled
    | Nullable (loc, e, an) ->
        Nullable (trans_type_expr e)
    | Shared (loc, e, an) -> error_at loc "unsupported: shared"
    | Wrap (loc, e, an) -> trans_type_expr e
    | Tvar (loc, name) -> error_at loc "unsupported: parametrized types"
    | Name (loc, (loc2, name, args), a) ->
        (match name with
         | "unit" -> Null
         | "bool" -> Boolean
         | "int" -> Integer
         | "float" -> Number
         | "string" -> String
         | _ -> Ref (make_id root_id_uri name)
        )
  in
  trans_type_expr x

let trans_item
    ~root_id_uri
    (Type (loc, (name, param, an), e) : module_item) : def =
  let id = make_id root_id_uri name in
  if param <> [] then
    error_at loc "unsupported: parametrized types";
  {
    id;
    description = None;
    type_expr = trans_type_expr ~root_id_uri e;
  }

let trans_full_module
    ?(root_id_uri = "/schemas")
    ~src_name
    ((_head, body) : full_module) : t =
  let defs = List.map (trans_item ~root_id_uri) body in
  {
    root_id = root_id_uri;
    schema = "https://json-schema.org/draft/2020-12/schema";
    root_description = sprintf "Translated by atdcat from %s" src_name;
    defs;
  }

(***************************************************************************)
(* Translation to JSON (because we don't have atdgen :-/ *)
(***************************************************************************)

let string s = `String s

(* optional field *)
let opt field_name f x =
  match x with
  | None -> []
  | Some x -> ["required", f x]

let make_type_property ~is_nullable name =
  if is_nullable then
    ("type", `List [ `String name; `String "null" ])
  else
    ("type", `String name)

let rec type_expr_to_assoc ?(is_nullable = false) (x : type_expr)
  : (string * json) list =
  match x with
  | Ref s ->
      [ "$ref", `String s ]
  | Null ->
      [ make_type_property ~is_nullable:false "null" ]
  | Boolean ->
      [ make_type_property ~is_nullable "boolean" ]
  | Integer ->
      [ make_type_property ~is_nullable "integer" ]
  | Number ->
      [ make_type_property ~is_nullable "number" ]
  | String ->
      [ make_type_property ~is_nullable "string" ]
  | Array x ->
      [
        make_type_property ~is_nullable "array";
        "items", type_expr_to_json x
      ]
  | Tuple xs ->
      [
        make_type_property ~is_nullable "array";
        "minItems", `Int (List.length xs);
        "additionalItems", `Bool false;
        "items", `List (List.map type_expr_to_json xs);
      ]
  | Object x ->
      let properties =
        List.map (fun (name, x) ->
          (name, type_expr_to_json x)
        ) x.properties
      in
      [
        make_type_property ~is_nullable "object";
        "required", `List (List.map string x.required);
        "properties", `Assoc properties;
      ]
  | Union xs ->
      [ "oneOf", `List (List.map (type_expr_to_json ~is_nullable) xs) ]
  | Nullable x ->
      type_expr_to_assoc ~is_nullable:true x
  | Const json ->
      [ "const", json ]

and type_expr_to_json ?(is_nullable = false) (x : type_expr) : json =
  `Assoc (type_expr_to_assoc ~is_nullable x)

let def_to_json (x : def) : json =
  `Assoc (List.flatten [
    ["$id", `String x.id];
    opt "description" string x.description;
    type_expr_to_assoc x.type_expr;
  ])

let to_json (x : t) : json =
  `Assoc [
    "$id", `String x.root_id;
    "$schema", `String x.schema;
    "description", `String x.root_description;
    "$defs", `List (List.map def_to_json x.defs);
  ]

let print ?(root_id_uri = "/schemas") ~src_name oc ast =
  ast
  |> trans_full_module ~root_id_uri ~src_name
  |> to_json
  |> Yojson.Safe.pretty_to_channel oc;
  output_char oc '\n'
