(*
   Python code generation for JSON support (no biniou support)

   Takes the contents of a .atd file and translates it to a .py file.

   Design:
   - Python's standard 'json' module handles the parsing into generic
     dictionaries and such.
   - The generated code assigns one class to each ATD record. The use
     of type annotations allows for some type checking with mypy.
   - When converting from JSON to Python, the well-formedness of the data
     is checked.
   - When converting from Python to JSON, the well-formedness of the data
     is checked as well since the type system is too easy to bypass.
   - Sum types use one main class and one subclass per case.
   - Tuples, like arrays, options, and nullables don't get a class of
     their own.
   - Generic functions are provided to deal with the case where the JSON
     root is an array.

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
  (* Global *)
  imports: Atd.Imports.t;
  create_variable: string -> string;
  translate_variable: string -> string;
  (* Local to a class: instance variables, including method names *)
  translate_inst_variable: unit -> (string -> string);
}

let annot_schema_python : Atd.Annot.schema_section =
  {
    section = "python";
    fields = [
      Module_head, "text";
      Module_head, "json_py.text";
      Type_def, "decorator";
      Type_expr, "repr";
      Field, "default";
    ]
  }

let annot_schema : Atd.Annot.schema =
  annot_schema_python :: Atd.Json.annot_schema_json

(* Translate a preferred variable name into an available Python identifier. *)
let trans env id =
  env.translate_variable id

(*
   Convert an ascii string to CamelCase.
   Note that this gets rid of leading and trailing underscores.
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
  let name = Buffer.contents buf in
  if name = "" then "X"
  else
    (* Make sure we don't start with a digit. This happens with
       generated identifiers like '_42'. *)
    match name.[0] with
    | 'A'..'Z' | 'a'..'z' | '_' -> name
    | _ -> "X" ^ name

(* Use CamelCase as recommended by PEP 8.

     foo_bar -> FooBar
     fiz.foo_bar -> fiz.FooBar
*)
let class_name (env : env) loc (name : type_name) =
  let import, base_name = Atd.Imports.resolve env.imports loc name in
  match import with
  | None -> trans env (to_camel_case base_name)
  | Some import ->
      (* Don't translate the base name 'bar' if it's fully qualified
         as in 'foo.bar' because it would not conflict with the definition
         of a new class 'bar'.
         We do however perform the camel-case conversion from the ATD
         name to the idiomatic Python name.
         TODO: allow overriding the imported module and the class name via
         ATD annotations.
      *)
      import.name ^ "." ^ (to_camel_case base_name)

(*
   Create a class identifier that hasn't been seen yet.
   This is for internal disambiguation and still must translated using
   the 'trans' function ('class_name' will not work due to trailing
   underscores being added for disambiguation).
*)
let create_class_name env name =
  let preferred_id = to_camel_case name in
  env.create_variable preferred_id

let init_env (imports : import list) (defs : type_def list) : env =
  let local_module_names = List.map (fun (x : import) -> x.name) imports in
  let imports = Atd.Imports.load imports in
  let keywords = [
    (* Keywords
       https://docs.python.org/3/reference/lexical_analysis.html#keywords
    *)
    "False"; "await"; "else"; "import"; "pass";
    "None"; "break"; "except"; "in"; "raise";
    "True"; "class"; "finally"; "is"; "return";
    "and"; "continue"; "for"; "lambda"; "try";
    "as"; "def"; "from"; "nonlocal"; "while";
    "assert"; "del"; "global"; "not"; "with";
    "async"; "elif"; "if"; "or"; "yield";

    (* Soft keywords
       https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
    *)
    "match"; "case"; "_";
  ]
  in
  (* Various variables used in the generated code.
     Lowercase variables in this list are superfluous as long as all generated
     variables either start with '_', 'atd_', or an uppercase letter.
  *)
  let reserved_variables = [
    (* from typing *)
    "Any"; "Callable"; "Dict"; "List"; "Optional"; "Tuple";

    (* for use in json.dumps, json.loads etc. *)
    "json";

    (* exceptions *)
    "ValueError";

    (* used to check JSON node type *)
    "isinstance";
    "bool"; "int"; "float"; "str"; "dict"; "list"; "tuple";

    (* other built-in variables *)
    "self"; "cls"; "repr";
  ] in
  let variables =
    Atd.Unique_name.init
      ~reserved_identifiers:(reserved_variables
                             @ keywords
                             @ local_module_names)
      ~reserved_prefixes:["atd_"; "_atd_"]
      ~safe_prefix:"x_"
  in
  let method_names () =
    Atd.Unique_name.init
      ~reserved_identifiers:(
        ["from_json"; "to_json";
         "from_json_string"; "to_json_string"]
        @ keywords
      )
      ~reserved_prefixes:["__"]
      ~safe_prefix:"x_"
  in
  let create_variable name =
    Atd.Unique_name.create variables name
  in
  let translate_variable id =
    Atd.Unique_name.translate variables id
  in
  let translate_inst_variable () =
    let u = method_names () in
    fun id -> Atd.Unique_name.translate u id
  in
  {
    imports;
    create_variable;
    translate_variable;
    translate_inst_variable;
  }

type quote_kind = Single | Double

(* Escape a string fragment to be placed in single quotes or double quotes.
   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
*)
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

let _double_esc s =
  escape_string_content Double s

let make_preamble ~atd_filename ~atd_imports =
  sprintf {|"""Generated by atdpy from type definitions in %s.

This implements classes for the types defined in '%s', providing
methods and functions to convert data from/to JSON.
"""

# Disable flake8 entirely on this file:
# flake8: noqa

# Import annotations to allow forward references
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, NoReturn, Optional, Tuple, Union

import json

# ATD modules
%s
############################################################################
# Private functions
############################################################################


def _atd_missing_json_field(type_name: str, json_field_name: str) -> NoReturn:
    raise ValueError(f"missing field '{json_field_name}'"
                     f" in JSON object of type '{type_name}'")


def _atd_bad_json(expected_type: str, json_value: Any) -> NoReturn:
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'

    raise ValueError(f"incompatible JSON value where"
                     f" type '{expected_type}' was expected: '{value_str}'")


def _atd_bad_python(expected_type: str, json_value: Any) -> NoReturn:
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'

    raise ValueError(f"incompatible Python value where"
                     f" type '{expected_type}' was expected: '{value_str}'")


def _atd_read_unit(x: Any) -> None:
    if x is None:
        return x
    else:
        _atd_bad_json('unit', x)


def _atd_read_bool(x: Any) -> bool:
    if isinstance(x, bool):
        return x
    else:
        _atd_bad_json('bool', x)


def _atd_read_int(x: Any) -> int:
    if isinstance(x, int):
        return x
    else:
        _atd_bad_json('int', x)


def _atd_read_float(x: Any) -> float:
    if isinstance(x, (int, float)):
        return x
    else:
        _atd_bad_json('float', x)


def _atd_read_string(x: Any) -> str:
    if isinstance(x, str):
        return x
    else:
        _atd_bad_json('str', x)


def _atd_read_list(
            read_elt: Callable[[Any], Any]
        ) -> Callable[[List[Any]], List[Any]]:
    def read_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [read_elt(elt) for elt in elts]
        else:
            _atd_bad_json('array', elts)
    return read_list


def _atd_read_assoc_array_into_dict(
            read_key: Callable[[Any], Any],
            read_value: Callable[[Any], Any],
        ) -> Callable[[List[Any]], Dict[Any, Any]]:
    def read_assoc(elts: List[List[Any]]) -> Dict[str, Any]:
        if isinstance(elts, list):
            return {read_key(elt[0]): read_value(elt[1]) for elt in elts}
        else:
            _atd_bad_json('array', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc


def _atd_read_assoc_object_into_dict(
            read_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], Dict[str, Any]]:
    def read_assoc(elts: Dict[str, Any]) -> Dict[str, Any]:
        if isinstance(elts, dict):
            return {_atd_read_string(k): read_value(v)
                    for k, v in elts.items()}
        else:
            _atd_bad_json('object', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc


def _atd_read_assoc_object_into_list(
            read_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], List[Tuple[str, Any]]]:
    def read_assoc(elts: Dict[str, Any]) -> List[Tuple[str, Any]]:
        if isinstance(elts, dict):
            return [(_atd_read_string(k), read_value(v))
                    for k, v in elts.items()]
        else:
            _atd_bad_json('object', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc


def _atd_read_nullable(read_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    def read_nullable(x: Any) -> Any:
        if x is None:
            return None
        else:
            return read_elt(x)
    return read_nullable


def _atd_write_unit(x: Any) -> None:
    if x is None:
        return x
    else:
        _atd_bad_python('unit', x)


def _atd_write_bool(x: Any) -> bool:
    if isinstance(x, bool):
        return x
    else:
        _atd_bad_python('bool', x)


def _atd_write_int(x: Any) -> int:
    if isinstance(x, int):
        return x
    else:
        _atd_bad_python('int', x)


def _atd_write_float(x: Any) -> float:
    if isinstance(x, (int, float)):
        return x
    else:
        _atd_bad_python('float', x)


def _atd_write_string(x: Any) -> str:
    if isinstance(x, str):
        return x
    else:
        _atd_bad_python('str', x)


def _atd_write_list(
            write_elt: Callable[[Any], Any]
        ) -> Callable[[List[Any]], List[Any]]:
    def write_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [write_elt(elt) for elt in elts]
        else:
            _atd_bad_python('list', elts)
    return write_list


def _atd_write_assoc_dict_to_array(
            write_key: Callable[[Any], Any],
            write_value: Callable[[Any], Any]
        ) -> Callable[[Dict[Any, Any]], List[Tuple[Any, Any]]]:
    def write_assoc(elts: Dict[str, Any]) -> List[Tuple[str, Any]]:
        if isinstance(elts, dict):
            return [(write_key(k), write_value(v)) for k, v in elts.items()]
        else:
            _atd_bad_python('Dict[str, <value type>]]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc


def _atd_write_assoc_dict_to_object(
            write_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], Dict[str, Any]]:
    def write_assoc(elts: Dict[str, Any]) -> Dict[str, Any]:
        if isinstance(elts, dict):
            return {_atd_write_string(k): write_value(v)
                    for k, v in elts.items()}
        else:
            _atd_bad_python('Dict[str, <value type>]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc


def _atd_write_assoc_list_to_object(
            write_value: Callable[[Any], Any],
        ) -> Callable[[List[Any]], Dict[str, Any]]:
    def write_assoc(elts: List[List[Any]]) -> Dict[str, Any]:
        if isinstance(elts, list):
            return {_atd_write_string(elt[0]): write_value(elt[1])
                    for elt in elts}
        else:
            _atd_bad_python('List[Tuple[<key type>, <value type>]]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc


def _atd_write_nullable(write_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    def write_nullable(x: Any) -> Any:
        if x is None:
            return None
        else:
            return write_elt(x)
    return write_nullable


############################################################################
# Public classes
############################################################################|}
    atd_filename
    atd_filename
    atd_imports

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdpy: " ^ msg)

let todo hint =
  failwith ("TODO: " ^ hint)

let spaced ?(spacer = [Line ""]) (blocks : B.node list) : B.node list =
  let rec spaced xs =
    match List.filter (fun x -> not (B.is_empty_node x)) xs with
    | []
    | [_] as xs -> xs
    | a :: rest -> a :: spacer @ spaced rest
  in
  spaced blocks

let double_spaced blocks =
  spaced ~spacer:[Line ""; Line ""] blocks

(*
   Representations of ATD type '(string * value) list' in JSON and Python.
   Key type or value type are provided when it's useful.
*)
type assoc_kind =
  | Array_list (* default representation; possibly not even a list of pairs *)
  | Array_dict of type_expr * type_expr (* key type, value type *)
  (* Keys in JSON objects are always of type string. *)
  | Object_dict of type_expr (* value type *)
  | Object_list of type_expr (* value type *)

let assoc_kind loc (e : type_expr) an : assoc_kind =
  let json_repr = Atd.Json.get_json_list an in
  let python_repr = Python_annot.get_python_assoc_repr an in
  match e, json_repr, python_repr with
  | Tuple (loc, [(_, key, _); (_, value, _)], an2), Array, Dict ->
      Array_dict (key, value)
  | Tuple (loc,
           [(_, Name (_, (_, TN ["string"], _), _), _); (_, value, _)], an2),
    Object, Dict ->
      Object_dict value
  | Tuple (loc,
           [(_, Name (_, (_, TN ["string"], _), _), _); (_, value, _)], an2),
    Object, List -> Object_list value
  | _, Array, List -> Array_list
  | _, Object, _ -> error_at loc "not a (string * _) list"
  | _, Array, _ -> error_at loc "not a (_ * _) list"

(* Map ATD built-in types to built-in mypy types *)
let py_type_name (env : env) loc (name : type_name) =
  match name with
  | TN ["unit"] -> "None"
  | TN ["bool"] -> "bool"
  | TN ["int"] -> "int"
  | TN ["float"] -> "float"
  | TN ["string"] -> "str"
  | TN ["abstract"] -> "Any"
  | user_defined -> class_name env loc name

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, xs, an) ->
      let type_names =
        xs
        |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
      in
      sprintf "Tuple[%s]" (String.concat ", " type_names)
  | List (loc, e, an) ->
     (match assoc_kind loc e an with
       | Array_list
       | Object_list _ ->
           sprintf "List[%s]"
             (type_name_of_expr env e)
       | Array_dict (key, value) ->
           sprintf "Dict[%s, %s]"
             (type_name_of_expr env key) (type_name_of_expr env value)
       | Object_dict value ->
           sprintf "Dict[str, %s]"
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "Optional[%s]" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "Optional[%s]" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> todo "wrap"
  | Name (loc, (loc2, name, []), an) -> py_type_name env loc2 name
  | Name (loc, (_, name, _::_), _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

let rec get_default_default
    ?(mutable_ok = true) env (e : type_expr) : string option =
  match e with
  | Sum _
  | Record _
  | Tuple _ (* a default tuple could be possible but we're lazy *) -> None
  | List _ ->
      if mutable_ok then Some "[]"
      else None
  | Option _
  | Nullable _ -> Some "None"
  | Shared (loc, e, an) -> get_default_default ~mutable_ok env e
  | Wrap (loc, e, an) -> get_default_default ~mutable_ok env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["unit"] -> Some "None"
       | TN ["bool"] -> Some "False"
       | TN ["int"] -> Some "0"
       | TN ["float"] -> Some "0.0"
       | TN ["string"] -> Some {|""|}
       | TN ["abstract"] -> Some "None"
       | _ -> assert false
      )
  | Name _ -> assert false
  | Tvar _ -> assert false

let get_python_default
    ?mutable_ok
    env (e : type_expr) (an : annot) : string option =
  let user_default = Python_annot.get_python_default an in
  match user_default with
  | Some s -> Some s
  | None -> get_default_default ?mutable_ok env e

(* see explanation where this function is used *)
let has_no_class_inst_prop_default
    env
    ((loc, (name, kind, an), e) : simple_field) =
  match kind with
  | Required -> true
  | Optional -> (* default is None *) false
  | With_default ->
      match get_python_default env ~mutable_ok:false e an with
      | Some _ -> false
      | None ->
          (* There's either no default at all which is an error,
             or the default value is known to be mutable. *)
          true

(* If the field is '?foo: bar option', its python or json value has type
   'bar' rather than 'bar option'. *)
let unwrap_field_type loc field_name kind e =
  match kind with
  | Required
  | With_default -> e
  | Optional ->
      match e with
      | Option (loc, e, an) -> e
      | e ->
          A.error_at loc
            (sprintf "the type of optional field '%s' should be of \
                      the form 'xxx option'"
               field_name)

(*
   Instance variable that's really the name of the getter method created
   by @dataclass. It can't start with '__' as those are reserved for
   internal magic. The 'trans_meth' translator must take care of this.
*)
let inst_var_name trans_meth field_name =
  trans_meth field_name

let rec json_writer env e =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> tuple_writer env cells
  | List (loc, e, an) ->
      (match assoc_kind loc e an with
       | Array_list ->
           sprintf "_atd_write_list(%s)" (json_writer env e)
       | Array_dict (key, value) ->
           sprintf "_atd_write_assoc_dict_to_array(%s, %s)"
             (json_writer env key) (json_writer env value)
       | Object_dict value ->
           sprintf "_atd_write_assoc_dict_to_object(%s)"
             (json_writer env value)
       | Object_list value ->
           sprintf "_atd_write_assoc_list_to_object(%s)"
             (json_writer env value)
      )
  | Option (loc, e, an)
  | Nullable (loc, e, an) ->
      sprintf "_atd_write_nullable(%s)" (json_writer env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_writer env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["bool" | "int" | "float" | "string" as str] ->
           sprintf "_atd_write_%s" str
       | TN ["abstract"] -> "(lambda x: x)"
       | _ -> "(lambda x: x.to_json())")
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert python tuple to json list

   (lambda x: [write0(x[0]), write1(x[1])] if isinstance(x, tuple) else error())
*)
and tuple_writer env cells =
  let len = List.length cells in
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_writer env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "(lambda x: [%s] \
           if isinstance(x, tuple) and len(x) == %d \
           else _atd_bad_python('tuple of length %d', x))"
    tuple_body
    len len

let construct_json_field env trans_meth
    ((loc, (name, kind, an), e) : simple_field) =
  let unwrapped_type = unwrap_field_type loc name kind e in
  let writer_function = json_writer env unwrapped_type in
  let assignment =
    [
      Line (sprintf "res['%s'] = %s(self.%s)"
              (Atd.Json.get_json_fname name an |> single_esc)
              writer_function
              (inst_var_name trans_meth name))
    ]
  in
  match kind with
  | Required
  | With_default -> assignment
  | Optional ->
      [
        Line (sprintf "if self.%s is not None:"
                (inst_var_name trans_meth name));
        Block assignment
      ]

(*
   Function value that can be applied to a JSON node, converting it
   to the desired value.
*)
let rec json_reader env (e : type_expr) =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> tuple_reader env cells
  | List (loc, e, an) ->
      (* ATD lists of pairs can be represented as objects in JSON or
         as dicts in Python. All 4 combinations are supported.
         The default is to use JSON arrays and Python lists. *)
      (match assoc_kind loc e an with
       | Array_list ->
           sprintf "_atd_read_list(%s)"
             (json_reader env e)
       | Array_dict (key, value) ->
           sprintf "_atd_read_assoc_array_into_dict(%s, %s)"
             (json_reader env key) (json_reader env value)
       | Object_dict value ->
           sprintf "_atd_read_assoc_object_into_dict(%s)"
             (json_reader env value)
       | Object_list value ->
           sprintf "_atd_read_assoc_object_into_list(%s)"
             (json_reader env value)
      )
  | Option (loc, e, an)
  | Nullable (loc, e, an) ->
      sprintf "_atd_read_nullable(%s)" (json_reader env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_reader env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | TN ["bool" | "int" | "float" | "string" as str] ->
           sprintf "_atd_read_%s" str
       | TN ["abstract"] -> "(lambda x: x)"
       | _ -> sprintf "%s.from_json" (class_name env loc2 name))
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert json list to python tuple

   (lambda x: (read0(x[0]), read1(x[1])) if isinstance(x, list) else error())
*)
and tuple_reader env cells =
  let len = List.length cells in
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_reader env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "(lambda x: (%s) \
           if isinstance(x, list) and len(x) == %d \
           else _atd_bad_json('array of length %d', x))"
    tuple_body
    len len

let from_json_class_argument
    env trans_meth py_class_name ((loc, (name, kind, an), e) : simple_field) =
  let python_name = inst_var_name trans_meth name in
  let json_name = Atd.Json.get_json_fname name an in
  let unwrapped_type =
    match kind with
    | Required
    | With_default -> e
    | Optional ->
        match e with
        | Option (loc, e, an) -> e
        | _ ->
            A.error_at loc
              (sprintf "the type of optional field '%s' should be of \
                        the form 'xxx option'" name)
  in
  let else_body =
    match kind with
    | Required ->
        sprintf "_atd_missing_json_field('%s', '%s')"
          (single_esc py_class_name)
          (single_esc json_name)
    | Optional -> "None"
    | With_default ->
        match get_python_default env e an with
        | Some x -> x
        | None ->
            A.error_at loc
              (sprintf "missing default Python value for field '%s'"
                 name)
  in
  sprintf "%s=%s(x['%s']) if '%s' in x else %s,"
    python_name
    (json_reader env unwrapped_type)
    (single_esc json_name)
    (single_esc json_name)
    else_body

let inst_var_declaration
    env trans_meth ((loc, (name, kind, an), e) : simple_field) =
  let var_name = inst_var_name trans_meth name in
  let type_name = type_name_of_expr env e in
  let unwrapped_e = unwrap_field_type loc name kind e in
  let default =
    match kind with
    | Required -> ""
    | Optional -> " = None"
    | With_default ->
        match get_python_default ~mutable_ok:false env unwrapped_e an with
        | None -> ""
        | Some value -> sprintf " = %s" value
  in
  [
    Line (sprintf "%s: %s%s" var_name type_name default)
  ]

let record
    env ~class_decorators loc (name : type_name) (fields : field list) an =
  let py_class_name = class_name env loc name in
  let trans_meth = env.translate_inst_variable () in
  let fields =
    List.map (fun (x : field) ->
      match x with
      | Field x -> x
      | Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  (*
     Reorder fields with no-defaults first as required by @dataclass.
     Starting with Python 3.10, '@dataclass(kw_only=True)' solves this
     problem and makes this reordering unnecessary.
     TODO: remove once we require python >= 3.10. It could also be done
     as command-line flag specifying the Python version.
  *)
  let fields =
    let no_default, with_default =
      List.partition (has_no_class_inst_prop_default env) fields in
    no_default @ with_default
  in
  let inst_var_declarations =
    List.map (fun x ->
      Inline (inst_var_declaration env trans_meth x)) fields
  in
  let json_object_body =
    List.map (fun x ->
      Inline (construct_json_field env trans_meth x)) fields in
  let from_json_class_arguments =
    List.map (fun x ->
      Line (from_json_class_argument
              env trans_meth py_class_name x)
    ) fields in
  let from_json =
    [
      Line "@classmethod";
      Line (sprintf "def from_json(cls, x: Any) -> '%s':"
              (single_esc py_class_name));
      Block [
        Line "if isinstance(x, dict):";
        Block [
          Line "return cls(";
          Block from_json_class_arguments;
          Line ")"
        ];
        Line "else:";
        Block [
          Line (sprintf "_atd_bad_json('%s', x)"
                  (single_esc py_class_name))
        ]
      ]
    ]
  in
  let to_json =
    [
      Line "def to_json(self) -> Any:";
      Block [
        Line "res: Dict[str, Any] = {}";
        Inline json_object_body;
        Line "return res"
      ]
    ]
  in
  let from_json_string =
    [
      Line "@classmethod";
      Line (sprintf "def from_json_string(cls, x: str) -> '%s':"
              (single_esc py_class_name));
      Block [
        Line "return cls.from_json(json.loads(x))"
      ]
    ]
  in
  let to_json_string =
    [
      Line "def to_json_string(self, **kw: Any) -> str:";
      Block [
        Line "return json.dumps(self.to_json(), **kw)"
      ]
    ]
  in
  [
    Inline class_decorators;
    Line (sprintf "class %s:" py_class_name);
    Block (spaced [
      Line (sprintf {|"""Original type: %s = { ... }"""|} (Atd.Print.tn name));
      Inline inst_var_declarations;
      Inline from_json;
      Inline to_json;
      Inline from_json_string;
      Inline to_json_string;
    ])
  ]

(*
   A general-purpose wrapper that provides json-related methods for a type.
   This is used for tuples and for type aliases e.g. 'type foo = bar array'.

class Foo:
  def __init__(self, x: T):
    ...
  def to_json(self):
    ...
  def from_json(x):
    ...
  def to_json_string(self):
    ...
  def from_json_string(x):
    ...
*)
let alias_wrapper env ~class_decorators loc name type_expr =
  let py_class_name = class_name env loc name in
  let value_type = type_name_of_expr env type_expr in
  [
    Inline class_decorators;
    Line (sprintf "class %s:" py_class_name);
    Block [
      Line (sprintf {|"""Original type: %s"""|} (Atd.Print.tn name));
      Line "";
      Line (sprintf "value: %s" value_type);
      Line "";
      Line "@classmethod";
      Line (sprintf "def from_json(cls, x: Any) -> '%s':"
              (single_esc py_class_name));
      Block [
        Line (sprintf "return cls(%s(x))" (json_reader env type_expr))
      ];
      Line "";
      Line "def to_json(self) -> Any:";
      Block [
        Line (sprintf "return %s(self.value)" (json_writer env type_expr))
      ];
      Line "";
      Line "@classmethod";
      Line (sprintf "def from_json_string(cls, x: str) -> '%s':"
              (single_esc py_class_name));
      Block [
        Line "return cls.from_json(json.loads(x))"
      ];
      Line "";
      Line "def to_json_string(self, **kw: Any) -> str:";
      Block [
        Line "return json.dumps(self.to_json(), **kw)"
      ]
    ]
  ]

let case_class env ~class_decorators (type_name : type_name)
    (loc, orig_name, unique_name, an, opt_e) =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match opt_e with
  | None ->
      [
        Inline class_decorators;
        Line (sprintf "class %s:" (trans env unique_name));
        Block [
          Line (sprintf {|"""Original type: %s = [ ... | %s | ... ]"""|}
                  (Atd.Print.tn type_name)
                  orig_name);
          Line "";
          Line "@property";
          Line "def kind(self) -> str:";
          Block [
            Line {|"""Name of the class representing this variant."""|};
            Line (sprintf "return '%s'" (trans env unique_name))
          ];
          Line "";
          Line "@staticmethod";
          Line "def to_json() -> Any:";
          Block [
            Line (sprintf "return '%s'" (single_esc json_name))
          ];
          Line "";
          Line "def to_json_string(self, **kw: Any) -> str:";
          Block [
            Line "return json.dumps(self.to_json(), **kw)"
          ]
        ]
      ]
  | Some e ->
      [
        Inline class_decorators;
        Line (sprintf "class %s:" (trans env unique_name));
        Block [
          Line (sprintf {|"""Original type: %s = [ ... | %s of ... | ... ]"""|}
                  (Atd.Print.tn type_name)
                  orig_name);
          Line "";
          Line (sprintf "value: %s" (type_name_of_expr env e));
          Line "";
          Line "@property";
          Line "def kind(self) -> str:";
          Block [
            Line {|"""Name of the class representing this variant."""|};
            Line (sprintf "return '%s'" (trans env unique_name))
          ];
          Line "";
          Line "def to_json(self) -> Any:";
          Block [
            Line (sprintf "return ['%s', %s(self.value)]"
                    (single_esc json_name)
                    (json_writer env e))
          ];
          Line "";
          Line "def to_json_string(self, **kw: Any) -> str:";
          Block [
            Line "return json.dumps(self.to_json(), **kw)"
          ]
        ]
      ]

let read_cases0 env loc name cases0 =
  let ifs =
    cases0
    |> List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
      let json_name = Atd.Json.get_json_cons orig_name an in
      Inline [
        Line (sprintf "if x == '%s':" (single_esc json_name));
        Block [
          Line (sprintf "return cls(%s())" (trans env unique_name))
        ]
      ]
    )
  in
  [
    Inline ifs;
    Line (sprintf "_atd_bad_json('%s', x)"
            (class_name env loc name |> single_esc))
  ]

let read_cases1 env loc name cases1 =
  let ifs =
    cases1
    |> List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
      let e =
        match opt_e with
        | None -> assert false
        | Some x -> x
      in
      let json_name = Atd.Json.get_json_cons orig_name an in
      Inline [
        Line (sprintf "if cons == '%s':" (single_esc json_name));
        Block [
          Line (sprintf "return cls(%s(%s(x[1])))"
                  (trans env unique_name)
                  (json_reader env e))
        ]
      ]
    )
  in
  [
    Inline ifs;
    Line (sprintf "_atd_bad_json('%s', x)"
            (class_name env loc name |> single_esc))
  ]

let sum_container env ~class_decorators loc (name : type_name) cases =
  let py_class_name = class_name env loc name in
  let type_list =
    List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
      trans env unique_name
    ) cases
    |> String.concat ", "
  in
  let cases0, cases1 =
    List.partition (fun (loc, orig_name, unique_name, an, opt_e) ->
      opt_e = None
    ) cases
  in
  let cases0_block =
    if cases0 <> [] then
      [
        Line "if isinstance(x, str):";
        Block (read_cases0 env loc name cases0)
      ]
    else
      []
  in
  let cases1_block =
    if cases1 <> [] then
      [
        Line "if isinstance(x, List) and len(x) == 2:";
        Block [
          Line "cons = x[0]";
          Inline (read_cases1 env loc name cases1)
        ]
      ]
    else
      []
  in
  [
    Inline class_decorators;
    Line (sprintf "class %s:" py_class_name);
    Block [
      Line (sprintf {|"""Original type: %s = [ ... ]"""|} (Atd.Print.tn name));
      Line "";
      Line (sprintf "value: Union[%s]" type_list);
      Line "";
      Line "@property";
      Line "def kind(self) -> str:";
      Block [
        Line {|"""Name of the class representing this variant."""|};
        Line (sprintf "return self.value.kind")
      ];
      Line "";
      Line "@classmethod";
      Line (sprintf "def from_json(cls, x: Any) -> '%s':"
              (single_esc py_class_name));
      Block [
        Inline cases0_block;
        Inline cases1_block;
        Line (sprintf "_atd_bad_json('%s', x)"
                (single_esc (class_name env loc name)))
      ];
      Line "";
      Line "def to_json(self) -> Any:";
      Block [
        Line "return self.value.to_json()";
      ];
      Line "";
      Line "@classmethod";
      Line (sprintf "def from_json_string(cls, x: str) -> '%s':"
              (single_esc py_class_name));
      Block [
        Line "return cls.from_json(json.loads(x))"
      ];
      Line "";
      Line "def to_json_string(self, **kw: Any) -> str:";
      Block [
        Line "return json.dumps(self.to_json(), **kw)"
      ]
    ]
  ]

let sum env ~class_decorators loc (name : type_name) cases =
  let cases =
    List.map (fun (x : variant) ->
      match x with
      | Variant (loc, (orig_name, an), opt_e) ->
          let unique_name = create_class_name env orig_name in
          (loc, orig_name, unique_name, an, opt_e)
      | Inherit _ -> assert false
    ) cases
  in
  let case_classes =
    List.map (fun x -> Inline (case_class env ~class_decorators name x)) cases
    |> double_spaced
  in
  let container_class = sum_container env ~class_decorators loc name cases in
  [
    Inline case_classes;
    Inline container_class;
  ]
  |> double_spaced

let uses_dataclass_decorator =
  let rex = Re.Pcre.regexp {|\A[ \t\r\n]*dataclass(\(|[ \t\r\n]|\z)|} in
  fun s -> Re.Pcre.pmatch ~rex s

let get_class_decorators an =
  let decorators = Python_annot.get_python_decorators an in
  (* Avoid duplicate use of the @dataclass decorator, which doesn't work
     if some options like frozen=True are used. *)
  if List.exists uses_dataclass_decorator decorators then
    decorators
  else
    decorators @ ["dataclass"]

let type_def env (x : A.type_def) : B.t =
  let loc = x.loc in
  let name = x.name in
  if x.param <> [] then
    not_implemented loc "parametrized type";
  let class_decorators =
    get_class_decorators x.annot
    |> List.map (fun s -> Line ("@" ^ s))
  in
  let rec unwrap e =
    match e with
    | Sum (loc, cases, an) ->
        sum env ~class_decorators loc name cases
    | Record (loc, fields, an) ->
        record env ~class_decorators loc name fields an
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Name _ -> alias_wrapper env ~class_decorators loc name e
    | Shared _ -> not_implemented loc "cyclic references"
    | Wrap (loc, e, an) -> unwrap e
    | Tvar _ -> not_implemented loc "parametrized type"
  in
  unwrap x.value

let definition_group ~atd_filename env
    (_is_recursive, (defs: A.type_def list)) : B.t =
  List.fold_left (fun acc x ->
    Inline (type_def env x) :: acc) [] defs
  |> List.rev
  |> spaced

(*
   Make sure that the types as defined in the atd file get a good name.
   For example, type 'foo' should become class 'Foo'.
   We do this because each case constructor of sum types will also
   translate to a class in the same namespace. For example,
   there may be a type like 'type bar = [ Foo | Bleep ]'.
   We want to ensure that the type 'foo' gets the name 'Foo' and that only
   later the case 'Foo' gets a lesser name like 'Foo_' or 'Foo2'.
*)
let reserve_good_class_names env (defs: type_def list) =
  List.iter
    (fun (x : type_def) -> ignore (class_name env x.loc x.name))
    defs

(* TODO: support ATD annotations that specify alternate module paths or names
   for Python modules. *)
let format_import env (x : import) =
  let path =
    List.map (trans env) x.path
    |> String.concat "."
  in
  let as_ =
    match x.alias with
    | None -> ""
    | Some alias -> sprintf " as %s" alias
  in
  sprintf "import %s%s" path as_

let format_imports env imports =
  List.map (fun x -> format_import env x ^ "\n") imports
  |> String.concat ""

let to_file ~atd_filename ~head ~imports (defs : A.type_def list) dst_path =
  let env = init_env imports defs in
  reserve_good_class_names env defs;
  let head = List.map (fun s -> Line s) head in
  let atd_imports = format_imports env imports in
  let python_defs =
    Atd.Util.tsort defs
    |> List.map (fun x -> Inline (definition_group ~atd_filename env x))
  in
  let preamble = make_preamble ~atd_filename ~atd_imports in
  [
    Line preamble;
    Inline head;
  ] @ python_defs
  |> double_spaced
  |> Indent.to_file ~indent:4 dst_path

let run_file src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ ".py"
    |> String.lowercase_ascii
  in
  let dst_path = dst_name in
  let module_ =
    Atd.Util.load_file
      ~annot_schema
      ~expand:true (* monomorphization = eliminate parametrized type defs *)
      ~keep_builtins:true
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let module_ = Atd.Ast.use_only_specific_variants module_ in
  let head = Python_annot.get_python_json_text (snd module_.module_head) in
  to_file ~atd_filename:src_name ~head ~imports:module_.imports
    module_.type_defs dst_path
