(*
   TypeScript code generation for JSON support (no biniou support)

   Takes the contents of a .atd file and translates it to a .ts file.
   Look into the tests to see what generated code looks like.
*)

open Printf
open Atd.Ast
open Indent
module A = Atd.Ast
module B = Indent

(* Mutable environment holding hash tables and such to avoid
   naming conflicts. *)
type env = {
  imports: Atd.Imports.t;
  create_variable: string -> string;
  translate_variable: string -> string;
}

let annot_schema_ts : Atd.Annot.schema_section =
  {
    section = "ts";
    fields = [
      Type_expr, "repr";
      Field, "default";
    ]
  }

let annot_schema : Atd.Annot.schema =
  annot_schema_ts :: Atd.Json.annot_schema_json

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdts: " ^ msg)

let todo hint =
  failwith ("TODO: " ^ hint)

(* This is used to convince the TypeScript type checker in strict mode
   that a function doesn't lack a return statement (TS2366) *)
let impossible = "throw new Error('impossible')"

(*
   Convert an ascii string to CamelCase.
   Note that this gets rid of leading and trailing underscores.

   TODO: share the implementation with atdpy?
*)
let to_camel_case s =
  let buf = Buffer.create (String.length s) in
  let start_word = ref true in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '_' ->
        start_word := true
    | 'a'..'z' as c when !start_word ->
        Buffer.add_char buf (Char.uppercase_ascii c);
        start_word := false
    | c ->
        Buffer.add_char buf c;
        start_word := false
  done;
  Buffer.contents buf

(* Translate a preferred variable name into an available TypeScript identifier. *)
let trans env id =
  env.translate_variable id

(* Use CamelCase as customary for type names. *)
let type_name env loc (name : type_name) =
  let import, base_name = Atd.Imports.resolve env.imports loc name in
  match import with
  | None -> trans env (to_camel_case base_name)
  | Some import -> not_implemented loc "imports"

let writer_name _env loc name =
  match name with
  | TN [str] -> "write" ^ to_camel_case str
  | _ -> not_implemented loc "imports"

let reader_name _env loc name =
  match name with
  | TN [str] -> "read" ^ to_camel_case str
  | _ -> not_implemented loc "imports"

(* Insert blank lines *)
let spaced ?(spacer = [Line ""]) (blocks : B.node list) : B.node list =
  let rec spaced xs =
    match List.filter (fun x -> not (B.is_empty_node x)) xs with
    | []
    | [_] as xs -> xs
    | a :: rest -> a :: spacer @ spaced rest
  in
  spaced blocks

(*
   Eliminate the 'wrap' constructs since we don't do anything with them,
   and produce decent error messages for the unsupported constructs.
*)
let rec unwrap e =
  match e with
  | Wrap (loc, e, an) -> unwrap e
  | Shared (loc, e, an) -> not_implemented loc "cyclic references"
  | Tvar _
  | Sum _
  | Record _
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ -> e

let init_env (imports : import list) : env =
  let imports = Atd.Imports.load imports in
  (* The list of "keywords" is extracted from
     https://github.com/microsoft/TypeScript/issues/2536#issuecomment-87194347
     In the current implementation, we don't use variables named by the
     user. Field names can be any alphanumeric identifiers in JS.
     Avoiding reserved words for field names is useful when the user wants
     to use them as variables, for example when they use the shorthand
     notation {foo} instead of {foo: foo}. In this case, the variable 'foo'
     may not be a reserved word.
  *)
  let keywords = [
   (* Reserved Words *)
    "break"; "case"; "catch"; "class"; "const"; "continue"; "debugger";
    "default"; "delete"; "do"; "else"; "enum"; "export"; "extends"; "false";
    "finally"; "for"; "function"; "if"; "import"; "in"; "instanceof";
    "new"; "null"; "return"; "super"; "switch"; "this"; "throw"; "true";
    "try"; "typeof"; "var"; "void"; "while"; "with";

    (* Strict Mode Reserved Words *)
    "as"; "implements"; "interface"; "let"; "package"; "private";
    "protected"; "public"; "static"; "yield";

    (* Contextual Keywords *)
    "any"; "boolean"; "constructor"; "declare"; "get"; "module";
    "require"; "number"; "set"; "string"; "symbol"; "type"; "from";
    "of";

    (* from https://github.com/microsoft/TypeScript/issues/2536#issuecomment-98259939 *)
    "async"; "await"; "namespace";

    (* Does anyone have complete list? Microsoft, I'm looking at you. *)
  ]
  in
  (* Various variables used in the generated code. *)
  let reserved_variables = [
    (* fill this thoroughly when we start using user-named variables *)
    "x"
  ] in
  let variables =
    Atd.Unique_name.init
      ~reserved_identifiers:(reserved_variables @ keywords)
      ~reserved_prefixes:["atd_"; "_atd_"]
      ~safe_prefix:"x_"
  in
  let create_variable name =
    Atd.Unique_name.create variables name
  in
  let translate_variable id =
    Atd.Unique_name.translate variables id
  in
  {
    imports;
    create_variable;
    translate_variable;
  }

type quote_kind = Single | Double

(* Escape a string fragment to be placed in single quotes or double quotes. *)
let escape_string_content quote_kind s =
  let buf = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    match s.[i], quote_kind with
    | '\n', _ -> Buffer.add_string buf "\\n"
    | '\\', _ -> Buffer.add_string buf "\\\\"
    | '\'', Single -> Buffer.add_string buf "\\'"
    | '"', Double -> Buffer.add_string buf "\\\""
    | c, (Single | Double) -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let single_esc s =
  escape_string_content Single s

let double_esc s =
  escape_string_content Double s

let runtime_start atd_filename =
  sprintf {|// Generated by atdts from type definitions in '%s'.
//
// Type-safe translations from/to JSON
//
// For each type 'Foo', there is a pair of functions:
// - 'writeFoo': convert a 'Foo' value into a JSON-compatible value.
// - 'readFoo': convert a JSON-compatible value into a TypeScript value
//   of type 'Foo'.
|}
    atd_filename

let runtime_end = {|
/////////////////////////////////////////////////////////////////////
// Runtime library
/////////////////////////////////////////////////////////////////////

export type Int = number

export type Option<T> = null | { value: T }

function _atd_missing_json_field(type_name: string, json_field_name: string) {
    throw new Error(`missing field '${json_field_name}'` +
                    ` in JSON object of type '${type_name}'`)
}

function _atd_missing_ts_field(type_name: string, ts_field_name: string) {
    throw new Error(`missing field '${ts_field_name}'` +
                    ` in TypeScript object of type '${type_name}'`)
}

function _atd_bad_json(expected_type: string, json_value: any, context: any) {
  let value_str = JSON.stringify(json_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible JSON value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_bad_ts(expected_type: string, ts_value: any, context: any) {
  let value_str = JSON.stringify(ts_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible TypeScript value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_check_json_tuple(len: Int, x: any, context: any) {
  if (! Array.isArray(x) || x.length !== len)
    _atd_bad_json('tuple of length ' + len, x, context);
}

function _atd_read_unit(x: any, context: any): null {
  if (x === null)
    return null
  else {
    _atd_bad_json('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_json('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_int(x: any, context: any): Int {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_json('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_json('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_json('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_required_field<T>(type_name: string,
                                     field_name: string,
                                     read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined) {
    _atd_missing_json_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return read_elt(x, context)
}

function _atd_read_optional_field<T>(read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined || x === null)
    return x
  else
    return read_elt(x, context)
}

function _atd_read_field_with_default<T>(read_elt: (x: any, context: any) => T,
                                         default_: T,
                                         x: any,
                                         context: any): T {
  if (x === undefined || x === null)
    return default_
  else
    return read_elt(x, context)
}

function _atd_read_option<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => Option<T> {
  function read_option(x: any, context: any): Option<T> {
    if (x === 'None')
      return null
    else {
      _atd_check_json_tuple(2, x, context);
      switch (x[0]) {
        case 'Some':
          return { value: read_elt(x[1], context) }
        default:
          _atd_bad_json('option', x, context)
          throw new Error('impossible')
      }
    }
  }
  return read_option
}

function _atd_read_nullable<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => T | null {
  function read_nullable(x: any, context: any): T | null {
    if (x === null)
      return null
    else
      return read_elt(x, context)
  }
  return read_nullable
}

function _atd_read_array<T>(read_elt: (x: any, context: any) => T):
  (elts: any, context: any) => T[] {
  function read_array(elts: any, context: any): T[] {
    if (Array.isArray(elts))
      return elts.map((x) => read_elt(x, elts))
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_array
}

function _atd_read_assoc_array_into_map<K, V>(
    read_key: (key: any, context: any) => K,
    read_value: (value: any, context: any) => V
  ): (x: any, context: any) => Map<K, V> {
  function read_assoc(elts: any, context: any): Map<K, V> {
    if (Array.isArray(elts)) {
      const res = new Map<K, V>([])
      for (const x of elts) {
        if (Array.isArray(x) && x.length === 2)
          res.set(read_key(x[0], x), read_value(x[1], x))
        else {
          _atd_bad_json('pair', x, elts)
          throw new Error('impossible')
        }
      }
      return res
    }
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_map<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => Map<string, T> {
  function read_assoc(elts: any, context: any): Map<string, T> {
    if (typeof elts === 'object') {
      const res = new Map<string, T>([])
      for (const [key, value] of Object.entries(elts))
        res.set(key, read_value(value, elts))
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_array<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => [string, T][] {
  function read_assoc(elts: any, context: any): [string, T][] {
    if (typeof elts === 'object') {
      const res: [string, T][] = []
      for (const [key, value] of Object.entries(elts))
        res.push([key, read_value(value, elts)])
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_write_unit(x: any, context: any) {
  if (x === null)
    return x
  else {
    _atd_bad_ts('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_ts('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_int(x: any, context: any): Int {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_ts('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_ts('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_ts('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_option<T>(write_elt: (x: T, context: any) => any):
   (elts: Option<T>, context: any) => any {
  function write_option(x: Option<T>, context: any): any {
    if (x === null)
      return 'None'
    else
      return ['Some', write_elt(x.value, context)]
  }
  return write_option
}

function _atd_write_nullable<T>(write_elt: (x: T, context: any) => any):
  (x: T | null, context: any) => any {
  function write_option(x: T | null, context: any): any {
    if (x === null)
      return null
    else
      return write_elt(x, context)
  }
  return write_option
}

function _atd_write_array<T>(write_elt: (elt: T, context: any) => any):
  (elts: T[], context: any) => any {
  return ((elts: T[], context: any): any =>
    elts.map((x) => write_elt(x, elts))
  )
}

function _atd_write_assoc_map_to_array<K, V>(
    write_key: (key: K, context: any) => any,
    write_value: (value: V, context: any) => any
  ): (elts: Map<K, V>, context: any) => any {
  function write_assoc(elts: Map<K, V>, context: any): any {
    const res: any = []
    elts.forEach((value: V, key: K) =>
      res.push([write_key(key, elts), write_value(value, elts)])
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_map_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: Map<string, T>, context: any) => any {
  function write_assoc(elts: Map<string, T>, context: any): any {
    const res: any = {}
    elts.forEach((value: T, key: string) =>
      res[key] = write_value(value, elts)
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_array_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: [string, T][], context: any) => any {
  function write_assoc(elts: [string, T][], context: any): any {
    const res: any = {}
    for (const [key, value] of elts)
      res[key] = write_value(value, elts)
    return res
  }
  return write_assoc
}

function _atd_write_required_field<T>(type_name: string,
                                      field_name: string,
                                      write_elt: (x: T, context: any) => any,
                                      x: T,
                                      context: any): any {
  if (x === undefined) {
    _atd_missing_ts_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return write_elt(x, context)
}

function _atd_write_optional_field<T>(write_elt: (x: T, context: any) => any,
                                      x: T,
                                      context: any): any {
  if (x === undefined || x === null)
    return x
  else
    return write_elt(x, context)
}

function _atd_write_field_with_default<T>(
  write_elt: (x: T, context: any) => any,
  default_: T,
  x: T,
  context: any
): T {
  const value = (x === undefined || x === null) ? default_ : x
  return write_elt(value, context)
}
|}

(*
   Representations of ATD type '(string * value) list' in JSON and TypeScript.
   Key type or value type are provided when it's useful.
*)
type assoc_kind =
  | Array_array (* default representation; possibly not even a list of pairs *)
  | Array_map of type_expr * type_expr (* key type, value type *)
  (* Keys in JSON objects are always of type string. *)
  | Object_map of type_expr (* value type *)
  | Object_array of type_expr (* value type *)

let assoc_kind loc (e : type_expr) an : assoc_kind =
  let json_repr = Atd.Json.get_json_list an in
  let ts_repr = TS_annot.get_ts_assoc_repr an in
  match e, json_repr, ts_repr with
  | Tuple (loc, [(_, key, _); (_, value, _)], an2), Array, Map ->
      Array_map (key, value)
  | Tuple (loc,
           [(_, Name (_, (_, TN ["string"], _), _), _); (_, value, _)], an2),
    Object, Map ->
      Object_map value
  | Tuple (loc,
           [(_, Name (_, (_, TN ["string"], _), _), _); (_, value, _)], an2),
    Object, Array -> Object_array value
  | _, Array, Array -> Array_array
  | _, Object, _ -> error_at loc "not a (string * _) list"
  | _, Array, _ -> error_at loc "not a (_ * _) list"

(* Map ATD built-in types to built-in TypeScript types *)
let ts_type_name env loc (name : type_name) =
  match name with
  | TN ["unit"] -> "Null"
  | TN ["bool"] -> "boolean"
  | TN ["int"] -> "Int"
  | TN ["float"] -> "number"
  | TN ["string"] -> "string"
  | TN ["abstract"] -> "any"
  | user_defined -> type_name env loc user_defined

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> type_name_of_tuple env cells
  | List (loc, e, an) ->
      (match assoc_kind loc e an with
       | Array_array
       | Object_array _ ->
           sprintf "%s[]"
             (type_name_of_expr env e)
       | Array_map (key, value) ->
           sprintf "Map<%s, %s>"
             (type_name_of_expr env key) (type_name_of_expr env value)
       | Object_map value ->
           sprintf "Map<string, %s>"
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "Option<%s>" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "(%s | null)" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> todo "wrap"
  | Name (loc, (loc2, name, []), an) -> ts_type_name env loc2 name
  | Name (loc, _, _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

and type_name_of_tuple env cells : string =
  let type_names =
    cells
    |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
  in
  sprintf "[%s]" (String.concat ", " type_names)

let rec get_default_default (e : type_expr) : string option =
  match e with
  | Sum _
  | Record _
  | Tuple _ (* a default tuple could be possible but we're lazy *) -> None
  | List _ -> Some "[]"
  | Option _ -> Some "null"
  | Nullable _ -> Some "null"
  | Shared (loc, e, an) -> get_default_default e
  | Wrap (loc, e, an) -> get_default_default e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["unit"] -> Some "null"
       | TN ["bool"] -> Some "false"
       | TN ["int"] -> Some "0"
       | TN ["float"] -> Some "0.0"
       | TN ["string"] -> Some {|""|}
       | TN ["abstract"] -> Some "null"
       | _ -> None
      )
  | Name _ -> None
  | Tvar _ -> None

let get_ts_default (e : type_expr) (an : annot) : string option =
  let user_default = TS_annot.get_ts_default an in
  match user_default with
  | Some s -> Some s
  | None -> get_default_default e

(* If the field is '?foo: bar option', its ts or json value has type
   'bar' rather than 'bar option'. *)
let unwrap_field_type loc field_name kind e =
  match kind with
  | Required
  | With_default -> e
  | Optional ->
      match e with
      | Option (loc, e, an) -> e
      | _ ->
          A.error_at loc
            (sprintf "the type of optional field '%s' should be of \
                      the form 'xxx option'" field_name)

let rec json_reader env e =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> tuple_reader env cells
  | List (loc, e, an) ->
      (* ATD lists of pairs can be represented as objects in JSON or
         as Maps in TypeScript. All 4 combinations are supported.
         The default is to use JSON arrays and TypeScript arrays. *)
      (match assoc_kind loc e an with
       | Array_array ->
           sprintf "_atd_read_array(%s)" (json_reader env e)
       | Array_map (key, value) ->
           sprintf "_atd_read_assoc_array_into_map(%s, %s)"
             (json_reader env key) (json_reader env value)
       | Object_map value ->
           sprintf "_atd_read_assoc_object_into_map(%s)"
             (json_reader env value)
       | Object_array value ->
           sprintf "_atd_read_assoc_object_into_array(%s)"
             (json_reader env value)
      )
  | Option (loc, e, an) -> sprintf "_atd_read_option(%s)" (json_reader env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_read_nullable(%s)" (json_reader env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_reader env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["bool" | "int" | "float" | "string" as str] ->
           sprintf "_atd_read_%s" str
       | TN ["abstract"] -> "((x: any): any => x)"
       | TN [_] -> reader_name env loc2 name
       | TN _ -> not_implemented loc2 "imports")
  | Name (loc, _, _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert json list to tuple
*)
and tuple_reader env cells =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i], x)" (json_reader env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "((x, context): %s => \
            { _atd_check_json_tuple(%d, x, context); return [%s] })"
    (type_name_of_tuple env cells)
    (List.length cells)
    tuple_body

let rec json_writer env e =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> tuple_writer env cells
  | List (loc, e, an) ->
      (match assoc_kind loc e an with
       | Array_array ->
           sprintf "_atd_write_array(%s)" (json_writer env e)
       | Array_map (key, value) ->
           sprintf "_atd_write_assoc_map_to_array(%s, %s)"
             (json_writer env key) (json_writer env value)
       | Object_map value ->
           sprintf "_atd_write_assoc_map_to_object(%s)"
             (json_writer env value)
       | Object_array value ->
           sprintf "_atd_write_assoc_array_to_object(%s)"
             (json_writer env value)
      )
  | Option (loc, e, an) -> sprintf "_atd_write_option(%s)" (json_writer env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_write_nullable(%s)" (json_writer env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_writer env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["bool" | "int" | "float" | "string" as str] ->
           sprintf "_atd_write_%s" str
       | TN ["abstract"] -> "((x: any): any => x)"
       | TN [_] -> writer_name env loc2 name
       | TN _ -> not_implemented loc2 "imports")
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert tuple to json list
*)
and tuple_writer env cells =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i], x)" (json_writer env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "((x, context) => [%s])"
    tuple_body

let field_def env ((loc, (name, kind, an), e) : simple_field) =
  let field_name = trans env name in
  let unwrapped_e = unwrap_field_type loc name kind e in
  let type_name = type_name_of_expr env unwrapped_e in
  let optional =
    match kind with
    | Required -> ""
    | Optional -> "?"
    | With_default -> ""
  in
  [
    Line (sprintf "%s%s: %s;" field_name optional type_name)
  ]

let record_type env loc (name : type_name) (fields : field list) an =
  let ts_type_name = type_name env loc name in
  let fields =
    List.map (fun (x : field) ->
      match x with
      | Field x -> x
      | Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  let field_defs =
    List.map (fun x -> Inline (field_def env x)) fields
  in
  [
    Line (sprintf "export type %s = {" ts_type_name);
    Block field_defs;
    Line "}";
  ]

let alias_type env loc (name : type_name) type_expr =
  let ts_type_name = type_name env loc name in
  let value_type = type_name_of_expr env type_expr in
  [
    Line (sprintf "export type %s = %s" ts_type_name value_type)
  ]

let string_of_case_name name =
  sprintf "'%s'" (escape_string_content Single name)

let case_type env type_name (loc, case_name, an, opt_e) =
  let comment =
    let json_name = Atd.Json.get_json_cons case_name an in
    if case_name <> json_name then
      sprintf " /* JSON: \"%s\" */" (double_esc json_name)
    else
      ""
  in
  match opt_e with
  | None ->
      [
        Line (sprintf "| { kind: %s%s }"
                (string_of_case_name case_name)
                comment)
      ]
  | Some e ->
      [
        Line (sprintf "| { kind: %s%s; value: %s }"
                (string_of_case_name case_name)
                comment
                (type_name_of_expr env e));
      ]

let flatten_variants variants =
  List.map (fun (x : variant) ->
    match x with
    | Variant (loc, (orig_name, an), opt_e) -> (loc, orig_name, an, opt_e)
    | Inherit _ -> assert false
  ) variants

let sum_type env loc name cases =
  let case_types =
    List.map (fun x -> Inline (case_type env name x)) cases
  in
  [
    Line (sprintf "export type %s =" (type_name env loc name));
    Inline case_types;
  ]

let make_type_def env (x : A.type_def) : B.t =
  if x.param <> [] then
    not_implemented x.loc "parametrized type";
  let name = x.name in
  match unwrap x.value with
  | Sum (loc, variants, an) ->
      sum_type env loc name (flatten_variants variants)
  | Record (loc, fields, an) ->
      record_type env loc name fields an
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ -> alias_type env x.loc name x.value
  | Shared (loc, e, an) -> assert false
  | Wrap (loc, e, an) -> assert false
  | Tvar _ -> assert false

let read_case env loc orig_name an opt_e =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match opt_e with
  | None ->
      [
        Line (sprintf "case '%s':" (single_esc json_name));
        Block [
          Line (sprintf "return { kind: '%s' }" (single_esc orig_name))
        ]
      ]
  | Some e ->
      [
        Line (sprintf "case '%s':" (single_esc json_name));
        Block [
          Line (sprintf "return { kind: '%s', value: %s(x[1], x) }"
                  (single_esc orig_name)
                  (json_reader env e))
        ]
      ]

let write_case env loc orig_name an opt_e =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match opt_e with
  | None ->
      [
        Line (sprintf "case '%s':" (single_esc orig_name));
        Block [
          Line (sprintf "return '%s'" (single_esc json_name))
        ]
      ]
  | Some e ->
      [
        Line (sprintf "case '%s':" (single_esc orig_name));
        Block [
          Line (sprintf "return ['%s', %s(x.value, x)]"
                  (single_esc json_name)
                  (json_writer env e))
        ]
      ]

let read_root_expr env ~ts_type_name e =
  match unwrap e with
  | Sum (loc, variants, an) ->
      let cases = flatten_variants variants in
      let cases0, cases1 =
        List.partition (fun (loc, orig_name, an, opt_e) -> opt_e = None) cases
      in
      let part0 =
        [
          Line "switch (x) {";
          Block (
            List.map
              (fun (loc, orig_name, an, opt_e) ->
                 read_case env loc orig_name an opt_e
              ) cases0
            |> List.flatten
          );
          Block [
            Line "default:";
            Block [
              Line (sprintf "_atd_bad_json('%s', x, context)"
                      (single_esc ts_type_name));
              Line impossible;
            ]
          ];
          Line "}";
        ]
      in
      let part1 =
        [
          Line "_atd_check_json_tuple(2, x, context)";
          Line "switch (x[0]) {";
          Block (
            List.map
              (fun (loc, orig_name, an, opt_e) ->
                 read_case env loc orig_name an opt_e
              ) cases1
            |> List.flatten
          );
          Block [
            Line "default:";
            Block [
              Line (sprintf "_atd_bad_json('%s', x, context)"
                      (single_esc ts_type_name));
              Line impossible
            ]
          ];
          Line "}";
        ]
      in
      (match cases0, cases1 with
       | _, [] -> (* pure enum *)
           part0
       | [], _ -> (* pure non-enum *)
           part1
       | _ ->
           [
             Line "if (typeof x === 'string') {";
             Block part0;
             Line "}";
             Line "else {";
             Block part1;
             Line "}"
           ]
      )

  | Record (loc, fields, an) ->
      let read_fields =
        List.map (fun (x : field) ->
          match x with
          | Inherit _ -> assert false
          | Field ((loc, (name, kind, an), e) : simple_field) ->
              let ts_name = trans env name in
              let json_name_lit =
                Atd.Json.get_json_fname name an |> single_esc
              in
              let unwrapped_e = unwrap_field_type loc name kind e in
              (match kind with
               | Required ->
                   Line (
                     sprintf "%s: _atd_read_required_field(\
                               '%s', '%s', %s, x['%s'], x),"
                       ts_name
                       (single_esc ts_type_name)
                       json_name_lit
                       (json_reader env unwrapped_e)
                       json_name_lit
                   )
               | Optional ->
                   Line (sprintf "%s: _atd_read_optional_field(%s, x['%s'], x),"
                           ts_name
                           (json_reader env unwrapped_e)
                           json_name_lit)
               | With_default ->
                   (match get_ts_default e an with
                    | None ->
                        A.error_at loc
                          "a default field value must be specified with \
                           <ts default=\"...\">"
                    | Some default ->
                        Line (sprintf "%s: _atd_read_field_with_default\
                                       (%s, %s, x['%s'], x),"
                                ts_name
                                (json_reader env unwrapped_e)
                                default
                                json_name_lit)
                   )
              )
        ) fields
      in
      [
        Line "return {";
        Block read_fields;
        Line "};";
      ]
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ as e ->
      [
        Line (sprintf "return %s(x, context);" (json_reader env e))
      ]
  | Shared (loc, e, an) -> assert false
  | Wrap (loc, e, an) -> assert false
  | Tvar _ -> assert false

let write_root_expr env ~ts_type_name e =
  match unwrap e with
  | Sum (loc, variants, an) ->
      let cases = flatten_variants variants in
      [
        Line "switch (x.kind) {";
        Block (List.map (fun (loc, orig_name, an, opt_e) ->
          Inline (write_case env loc orig_name an opt_e)
        ) cases);
        Line "}";
      ]
  | Record (loc, fields, an) ->
      let write_fields =
        List.map (fun (x : field) ->
          match x with
          | Inherit _ -> assert false
          | Field ((loc, (name, kind, an), e) : simple_field) ->
              let ts_name = trans env name in
              let json_name_lit =
                sprintf "'%s'"
                  (Atd.Json.get_json_fname name an |> single_esc)
              in
              let unwrapped_e = unwrap_field_type loc name kind e in
              (match kind with
               | Required ->
                   Line (sprintf "%s: _atd_write_required_field\
                                    ('%s', '%s', %s, x.%s, x),"
                           json_name_lit
                           (single_esc ts_type_name)
                           (single_esc name)
                           (json_writer env unwrapped_e)
                           ts_name)
               | Optional ->
                   Line (sprintf "%s: _atd_write_optional_field\
                                    (%s, x.%s, x),"
                           json_name_lit
                           (json_writer env unwrapped_e)
                           ts_name)
               | With_default ->
                   let ts_default =
                     match get_ts_default e an with
                     | None ->
                         A.error_at loc
                          "a default field value must be specified with \
                           <ts default=\"...\">"
                     | Some x -> x
                   in
                   Line (sprintf "%s: _atd_write_field_with_default\
                                    (%s, %s, x.%s, x),"
                           json_name_lit
                           (json_writer env unwrapped_e)
                           ts_default
                           ts_name)
              )
        ) fields
      in
      [
        Line "return {";
        Block write_fields;
        Line "};";
      ]
  | Tuple _
  | List _
  | Option _
  | Nullable _
  | Name _ as e ->
      [
        Line (sprintf "return %s(x, context);" (json_writer env e))
      ]
  | Shared (loc, e, an) -> assert false
  | Wrap (loc, e, an) -> assert false
  | Tvar _ -> assert false

let make_reader env loc name an e =
  let ts_type_name = type_name env loc name in
  let ts_name = reader_name env loc name in
  let read = read_root_expr env ~ts_type_name e in
  [
    Line (sprintf "export function %s(x: any, context: any = x): %s {"
            ts_name ts_type_name);
    Block read;
    Line "}";
  ]

let make_writer env loc name an e =
  let ts_type_name = type_name env loc name in
  let ts_name = writer_name env loc name in
  let write = write_root_expr env ~ts_type_name e in
  [
    Line (sprintf "export function %s(x: %s, context: any = x): any {"
            ts_name ts_type_name);
    Block write;
    Line "}";
  ]

let make_functions env (x : A.type_def) : B.t =
  if x.param <> [] then
    assert false;
  let writer = make_writer env x.loc x.name x.annot x.value in
  let reader = make_reader env x.loc x.name x.annot x.value in
  [
    Inline writer;
    Line "";
    Inline reader;
  ]

(*
   Make sure that the types as defined in the atd file get a good name.
   For example, type 'foo' should become class 'Foo'.
   We do this because each case constructor of sum types will also
   translate to a class in the same namespace. For example,
   there may be a type like 'type bar = [ Foo | Bleep ]'.
   We want to ensure that the type 'foo' gets the name 'Foo' and that only
   later the case 'Foo' gets a lesser name like 'Foo_' or 'Foo2'.
*)
let reserve_good_type_names env (defs: A.type_def list) =
  List.iter
    (fun (x : type_def) -> ignore (type_name env x.loc x.name))
    defs

let to_file ~atd_filename imports (defs : A.type_def list) dst_path =
  let env = init_env imports in
  reserve_good_type_names env defs;
  let type_defs =
    List.map (fun x -> Inline (make_type_def env x)) defs
  in
  let functions =
    List.map (fun x ->
      Inline (make_functions env x)
    ) defs
  in
  [
    Line (runtime_start atd_filename);
    Inline (spaced type_defs);
    Inline (spaced functions);
    Line runtime_end;
  ]
  |> spaced
  |> Indent.to_file ~indent:2 dst_path

let run_file src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ ".ts"
    |> String.lowercase_ascii
  in
  let dst_path = dst_name in
  let module_ =
    Atd.Util.load_file
      ~annot_schema
      ~expand:true (* monomorphization *)
      ~keep_builtins:true
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let module_ = Atd.Ast.use_only_specific_variants module_ in
  if module_.imports <> [] then
    failwith "not implemented: import";
  to_file ~atd_filename:src_name module_.imports module_.type_defs dst_path
