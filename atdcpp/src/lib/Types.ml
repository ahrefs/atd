open Indent
open Util
open Printf
open Atd.Ast
module B = Indent

let unwrap_field_type loc field_name kind e = 
  match kind with
  | Required -> e
  | With_default -> e
  | Optional ->
    match e with
    | Option (loc, e, an) -> e
    | _ -> A.error_at loc (sprintf "the type of optional field '%s' should be of the form xxx option" field_name)

let rec unwrap e =
  match e with
  | Wrap (loc, e, an) -> unwrap e
  | Shared (loc, e, an) -> not_implemented loc "cyclic references"
  | Tvar (loc, name) -> not_implemented loc "parametrized type"
  | Sum _
  | Record _
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ -> e
    
let cpp_type_name env loc (name : string) = 
  match name with 
  | "unit" -> "void"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "float"
  | "string" -> "std::string"
  | "abstract" -> not_implemented loc "abstract types are not implemented"
  | user_defined -> Env.type_name env user_defined

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | List (loc, _, _) -> not_implemented loc "list"
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> 
    let cell_type_names = 
      cells |> List.map
    (fun (loc, type_expr, annot) -> type_name_of_expr env type_expr) 
    in
    sprintf "std::tuple<%s>" (Util.join_types cell_type_names)
  | Option (loc, e, an) -> sprintf "std::optional<%s>" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "std::unique_ptr<%s>" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> not_implemented loc "wrap"
  | Name (loc, (loc2, name, []), an) -> cpp_type_name env loc name
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

let rec type_default (e : type_expr) : string option = 
  match e with 
  | Sum _
  | Record _
  | Tuple _ 
  | List _ -> Some "{}"
  | Option _ -> Some "std::nullopt"
  | Nullable _ -> Some "nullptr";
  | Shared (loc, e, an) -> type_default e
  | Wrap (loc, e, an) -> type_default e
  | Name (loc, (loc2, name, []), an) ->
    (match name with
      | "unit" -> Some "void"
      | "bool" -> Some "false"
      | "int" -> Some "0"
      | "float" -> Some "0.0f"
      | "string" -> Some {|""|}
      | "abstract" -> None
      | _ -> None
    )
  | Name _ -> None
  | Tvar _ -> None 
        
  let get_cpp_default (e : type_expr) (an : annot) : string option =
    let user_default = CPPAnnot.get_cpp_default an in
    match user_default with
    | Some s -> Some s
    | None -> type_default e


let field_def env ((loc, (name, kind, an), e) : simple_field) = 
  let default = get_cpp_default e an in
  let field_name = Env.type_name env name in
  let unwrapped_e = unwrap_field_type loc name kind e in
  let type_name = type_name_of_expr env unwrapped_e in
  let string_end = match default with 
    | Some def -> sprintf " = %s;" def
    | None -> ";"
  in
  [
    Line (sprintf "%s %s%s" type_name field_name string_end)
  ]

let record_type env loc name (fields : field list) an =
  let cpp_type_name = Env.type_name env name in
  let fields = List.map(function 
    | `Field x -> x
    | `Inherit _ -> assert false)
    fields
  in
  let field_defs = 
    List.map (fun x -> Inline (field_def env x)) fields
  in 
  [
    Line (sprintf "struct %s \n{\npublic:" cpp_type_name);
    Block field_defs;
    Line "};";
  ]

let alias_type env name type_expr = 
  let cpp_type_name = Env.type_name env name in
  let value_type = type_name_of_expr env type_expr in
  [
    Line (sprintf "using %s = %s" cpp_type_name value_type)
  ]

let make_type_def env ((loc, (name, param, an), e) : A.type_def) : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  match unwrap e with 
  | Record (loc, fields, an) ->
      record_type env loc name fields an
  | Sum _ 
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ -> alias_type env name e
  | Shared (loc, e, an) -> assert false
  | Wrap (loc, e, an) -> assert false
  | Tvar _ -> assert false
