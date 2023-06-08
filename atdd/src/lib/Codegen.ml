(*
   Dlang code generation for JSON support (no biniou support)

   Takes the contents of a .atd file and translates it to a .d file.
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
  create_variable: string -> string;
  translate_variable: string -> string;
  (* Local to a class: instance variables, including method names *)
  translate_inst_variable: unit -> (string -> string);
}

let annot_schema_dlang : Atd.Annot.schema_section =
  {
    section = "dlang";
    fields = [
      Type_expr, "repr";
      Field, "default";
    ]
  }

let annot_schema : Atd.Annot.schema =
  annot_schema_dlang :: Atd.Json.annot_schema_json

(* Translate a preferred variable name into an available Dlang identifier. *)
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

(* Use CamelCase as recommended by PEP 8. *)
let class_name env id =
  trans env (to_camel_case id)

(*
   Create a class identifier that hasn't been seen yet.
   This is for internal disambiguation and still must translated using
   the 'trans' function ('class_name' will not work due to trailing
   underscores being added for disambiguation).
*)
let create_class_name env name =
  let preferred_id = to_camel_case name in
  env.create_variable preferred_id

(* TODO : edit list of keywoard based on dlang *)
let init_env () : env =
  let keywords = [
    (* Keywords
       https://dlang.org/spec/lex.html#keywords
    *)
    "abstract";"alias";"align";"asm";"assert";"auto";"body";"bool";
    "break";"byte";"case";"cast";"catch";"cdouble";"cent";"cfloat";
    "char";"class";"const";"continue";"creal";"dchar";"debug";"default";
    "delegate";"delete";"deprecated";"do";"double";"else";"enum";"export";
    "extern";"false";"final";"finally";"float";"for";"foreach";"foreach_reverse";
    "function";"goto";"idouble";"if";"ifloat";"immutable";"import";"in";
      "inout";"int";"interface";"invariant";"ireal";"is";"lazy";"long";"macro";
    "mixin";"module";"new";"nothrow";"null";"out";"override";"package";"pragma";
    "private";"protected";"public";"pure";"real";"ref";"return";"scope";"shared";
    "short";"static";"struct";"super";"switch";"synchronized";"template";"this";
    "throw";"true";"try";"typeid";"typeof";"ubyte";"ucent";"uint";"ulong";"union";
    "unittest";"ushort";"version";"void";"wchar";"while";"with";"__FILE__";"__FILE_FULL_PATH__";
    "__MODULE__";"__LINE__";"__FUNCTION__";"__PRETTY_FUNCTION__";"__gshared";
    "__traits";"__vector";"__parameters";
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
      ~reserved_identifiers:(reserved_variables @ keywords)
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

let fixed_size_preamble atd_filename =
  sprintf {|// """Generated by atdd from type definitions in %s.

  // This implements classes for the types defined in '%s', providing
  // methods and functions to convert data from/to JSON.
  // """
  
  // ############################################################################
  // # Private functions
  // ############################################################################
  
  import std.format;
  import std.conv;
  import std.json;
  import std.typecons : tuple, Tuple, nullable, Nullable;
  import std.array : array;
  import std.algorithm : map;
  
  class AtdException : Exception
  {
      this(string msg, string file = __FILE__, size_t line = __LINE__)
      {
          super(msg, file, line);
      }
  }
  
  void _atd_missing_json_field(string typeName, string jsonFieldName)
  {
      throw new AtdException("missing field %%s in JSON object of type %%s".format(typeName, jsonFieldName));
  }
  
  // TODO check later if template is right way to go
  AtdException _atd_bad_json(T)(string expectedType, T jsonValue)
  {
      string valueStr = jsonValue.to!string;
      if (valueStr.length > 200)
      {
          valueStr = valueStr[0 .. 200];
      }
  
      return new AtdException(
          "incompatible JSON value where type '%%s' was expected: %%s".format(
              expectedType, valueStr
      ));
  }
  
  AtdException _atd_bad_d(T)(string expectedType, T jsonValue)
  {
      string valueStr = jsonValue.to!string;
      if (valueStr.length > 200)
      {
          valueStr = valueStr[0 .. 200];
      }
  
      return new AtdException(
          "incompatible D value where type '%%s' was expected: %%s".format(
              expectedType, valueStr
      ));
  }
  
  typeof(null) _atd_read_unit(JSONValue x)
  {
      if (x.isNull)
          return null;
      else
          throw _atd_bad_json("unit", x);
  }
  
  bool _atd_read_bool(JSONValue x)
  {
      try
          return x.boolean;
      catch (JSONException e)
          throw _atd_bad_json("bool", x);
  }
  
  long _atd_read_int(JSONValue x)
  {
      try
          return x.integer;
      catch (JSONException e)
          throw _atd_bad_json("int", x);
  }
  
  float _atd_read_float(JSONValue x)
  {
      try
          return x.floating;
      catch (JSONException e)
          throw _atd_bad_json("float", x);
  }
  
  string _atd_read_string(JSONValue x)
  {
      try
          return x.str;
      catch (JSONException e)
          throw _atd_bad_json("string", x);
  }
  
  auto _atd_read_list(T)(T function(JSONValue) readElements)
  {
      return (JSONValue[] list) { return array(list.map!readElements()); };
  }
  
  auto _atd_read_assoc_array(V)(
      V function(JSONValue) readValue)
  {
      auto fun = (JSONValue[string] assocArr) {
          V[string] ret;
          foreach (key, val; assocArr)
              ret[key] = readValue(val);
          return ret;
      };
      return fun;
  }
  
  // TODO probably need to change that
  auto _atd_read_nullable(T)(T function(JSONValue) readElm)
  {
      auto fun = (JSONValue e) {
          if (e.isNull)
              return Nullable!T.init;
          else
              return Nullable!T(readElm(e));
      };
      return fun;
  }
  
  // this whole set of function could be remplaced by one templated _atd_write_value function
  // not sure it is what we want though
  
  JSONValue _atd_write_unit(typeof(null) n)
  {
      return JSONValue(null);
  }
  
  JSONValue _atd_write_bool(bool b)
  {
      return JSONValue(b);
  }
  
  JSONValue _atd_write_int(long i)
  {
      return JSONValue(i);
  }
  
  JSONValue _atd_write_float(float f)
  {
      return JSONValue(f);
  }
  
  JSONValue _atd_write_string(string s)
  {
      return JSONValue(s);
  }
  
  auto _atd_write_list(T)(JSONValue function(T) writeElm)
  {
      return (T[] list) { return JSONValue(array(list.map!writeElm())); };
  }
  
  auto _atd_write_assoc_array(T)(
      JSONValue function(T) writeValue)
  {
      auto fun = (T[string] assocArr) {
          JSONValue[string] ret;
          foreach (key, val; assocArr)
              ret[key] = writeValue(val);
          return JSONValue(ret);
      };
      return fun;
  }
  
  auto _atd_write_nullable(T)(JSONValue function(T) writeElm)
  {
      auto fun = (Nullable!T elm) {
          if (elm.isNull)
              return JSONValue(null);
          else
              return writeElm(elm.get);
      };
      return fun;
  }


  // ############################################################################
  // # Public classes
  // ############################################################################|}
    atd_filename
    atd_filename

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
   Representations of ATD type '(string * value) list' in JSON and Dlang.
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
  let dlang_repr = Dlang_annot.get_dlang_assoc_repr an in
  match e, json_repr, dlang_repr with
  | Tuple (loc, [(_, key, _); (_, value, _)], an2), Array, Dict ->
      Array_dict (key, value)
  | Tuple (loc,
           [(_, Name (_, (_, "string", _), _), _); (_, value, _)], an2),
    Object, Dict ->
      Object_dict value
  | Tuple (loc,
           [(_, Name (_, (_, "string", _), _), _); (_, value, _)], an2),
    Object, List -> Object_list value
  | _, Array, List -> Array_list
  | _, Object, _ -> error_at loc "not a (string * _) list"
  | _, Array, _ -> error_at loc "not a (_ * _) list"

(* Map ATD built-in types to built-in Dlang types *)
let dlang_type_name env (name : string) =
  match name with
  | "unit" -> "void"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "float"
  | "string" -> "string"
  | "abstract" -> "abstract" (* TODO : figure out *)
  | user_defined -> class_name env user_defined

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, xs, an) ->
      let type_names =
        xs
        |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
      in
      sprintf "Tuple!(%s)" (String.concat ", " type_names)
  | List (loc, e, an) ->
     (match assoc_kind loc e an with
       | Array_list
       | Object_list _ ->
           sprintf "%s[]"
             (type_name_of_expr env e)
       | Array_dict (key, value) ->
           sprintf "%s[%s]"
             (type_name_of_expr env key) (type_name_of_expr env value)
       | Object_dict value ->
           sprintf "string[%s]" (* TODO : dubious*)
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "Nullable!%s" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "Nullable!%s" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared" (* TODO *)
  | Wrap (loc, e, an) -> todo "wrap"
  | Name (loc, (loc2, name, []), an) -> dlang_type_name env name
  | Name (loc, (_, name, _::_), _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

let rec get_default_default (e : type_expr) : string option =
  match e with
  | Sum _
  | Record _
  | Tuple _ (* a default tuple could be possible but we're lazy *) -> None
  | List _ -> Some "[]"
  | Option _
  | Nullable _ -> Some "null"
  | Shared (loc, e, an) -> get_default_default e
  | Wrap (loc, e, an) -> get_default_default e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "unit" -> Some "null"
       | "bool" -> Some "false"
       | "int" -> Some "0"
       | "float" -> Some "0.0"
       | "string" -> Some {|""|}
       | "abstract" -> Some "null"
       | _ -> None
      )
  | Name _ -> None
  | Tvar _ -> None

let get_dlang_default (e : type_expr) (an : annot) : string option =
  let user_default = Dlang_annot.get_dlang_default an in
  match user_default with
  | Some s -> Some s
  | None -> get_default_default e

(* If the field is '?foo: bar option', its python or json value has type
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
  | Tuple (loc, cells, an) -> tuple_writer env (loc, cells, an)
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
  | Option (loc, e, an) ->
      sprintf "_atd_write_option(%s)" (json_writer env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_write_nullable(%s)" (json_writer env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_writer env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" -> sprintf "_atd_write_%s" name
       | "abstract" -> "(lambda x: x)"
       | _ -> "(lambda x: x.to_json())")
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert dlang tuple to json list

   (lambda x: [write0(x[0]), write1(x[1])] if isinstance(x, tuple) else error())
*)
and tuple_writer env (loc, cells, an) =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_writer env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "(%s x) => array(%s)"
    (type_name_of_expr env (Tuple (loc, cells, an)))
    tuple_body


let construct_json_field env trans_meth
    ((loc, (name, kind, an), e) : simple_field) =
  let unwrapped_type = unwrap_field_type loc name kind e in
  let writer_function = json_writer env unwrapped_type in
  let assignment =
    [
      Line (sprintf "res[\"%s\"] = %s(this.%s);"
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
        Line (sprintf "if this.%s is not None:"
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
  | Option (loc, e, an) ->
      sprintf "_atd_read_option(%s)" (json_reader env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_read_nullable(%s)" (json_reader env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> json_reader env e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" -> sprintf "_atd_read_%s" name
       | "abstract" -> "(lambda x: x)"
       | _ -> sprintf "%s.from_json" (class_name env name))
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

(*
   Convert json list to python tuple

   (lambda x: (read0(x[0]), read1(x[1])) if isinstance(x, list) else error())
*)
and tuple_reader env cells =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_reader env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "(JSONValue x) => tuple(%s)" tuple_body

let from_json_class_argument
    env trans_meth py_class_name ((loc, (name, kind, an), e) : simple_field) =
  let dlang_name = inst_var_name trans_meth name in
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
        match get_dlang_default e an with
        | Some x -> x
        | None ->
            A.error_at loc
              (sprintf "missing default Python value for field '%s'"
                 name)
  in
  sprintf "obj.%s=%s(x['%s']) if '%s' in x else %s,"
    dlang_name
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
        match get_dlang_default unwrapped_e an with
        | None -> ""
        | Some x ->
            (* This constructs ensures that a fresh default value is
               evaluated for each class instanciation. It's important for
               default lists since Python lists are mutable. *)
            sprintf " = field(default_factory=lambda: %s)" x
  in
  [
    Line (sprintf "%s: %s%s" var_name type_name default)
  ]

let record env loc name (fields : field list) an =
  let dlang_struct_name = class_name env name in
  let trans_meth = env.translate_inst_variable () in
  let fields =
    List.map (function
      | `Field x -> x
      | `Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  let inst_var_declarations =
    List.map (fun x -> Inline (inst_var_declaration env trans_meth x)) fields
  in
  let json_object_body =
    List.map (fun x ->
      Inline (construct_json_field env trans_meth x)) fields in
  let from_json_class_arguments =
    List.map (fun x ->
      Line (from_json_class_argument env trans_meth dlang_struct_name x)
    ) fields in
  let from_json =
    [
      Line (sprintf "static %s fromJson(JSONValue x) {"
              (single_esc dlang_struct_name));
      Block
        ([Line (sprintf "%s obj;" dlang_struct_name) ] @ from_json_class_arguments @
      [Line "return obj;"]);
      Line "}";
    ]
  in
  let to_json =
    [
      Line "JSONValue toJson() {";
      Block [
        Line ("JSONValue res;");
        Inline json_object_body;
        Line "return res;"
      ];
      Line "}";
    ]
  in
  let from_json_string =
    [
      Line (sprintf "static %s fromJsonString(string x) {"
              (single_esc dlang_struct_name));
      Block [
        Line "JSONValue res = parseJSON(x);";
        Inline json_object_body;
        Line "return res;"
      ];
      Line "}";
    ]
  in
  let to_json_string =
    [
      Line (sprintf "string toJsonString(%s obj) {" dlang_struct_name);
      Block [
        Line ("JSONValue res;");
        Inline json_object_body;
        Line "return res.toString;"
      ];
      Line "}";
    ]
  in
  [
    Line (sprintf "struct %s {" dlang_struct_name);
    Block (spaced [
      Line (sprintf {|"""Original type: %s = { ... }"""|} name);
      Inline inst_var_declarations;
      Inline from_json;
      Inline to_json;
      Inline from_json_string;
      Inline to_json_string;
    ]);
    Line ("}");
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
let alias_wrapper env ~class_decorators name type_expr =
  let dlang_struct_name = class_name env name in
  let value_type = type_name_of_expr env type_expr in
  [
    Inline class_decorators;
    Line (sprintf "struct %s {" dlang_struct_name);
    Block [
      Line (sprintf {|"""Original type: %s"""|} name);
      Line "";
      Line (sprintf "value: %s" value_type);
      Line "";
      Line "@classmethod";
      Line (sprintf "def from_json(cls, x: Any) -> '%s':"
              (single_esc dlang_struct_name));
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
              (single_esc dlang_struct_name));
      Block [
        Line "return cls.from_json(json.loads(x))"
      ];
      Line "";
      Line "def to_json_string(self, **kw: Any) -> str:";
      Block [
        Line "return json.dumps(self.to_json(), **kw)"
      ];
      Line "}"
    ]
  ]

let case_class env ~class_decorators type_name
    (loc, orig_name, unique_name, an, opt_e) =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match opt_e with
  | None ->
      [
        Inline class_decorators;
        Line (sprintf "class %s:" (trans env unique_name));
        Block [
          Line (sprintf {|"""Original type: %s = [ ... | %s | ... ]"""|}
                  type_name
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
                  type_name
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
            (class_name env name |> single_esc))
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
            (class_name env name |> single_esc))
  ]

let sum_container env ~class_decorators loc name cases =
  let py_class_name = class_name env name in
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
      Line (sprintf {|"""Original type: %s = [ ... ]"""|} name);
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
                (single_esc (class_name env name)))
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

let sum env ~class_decorators loc name cases =
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
  let decorators = Dlang_annot.get_dlang_decorators an in
  (* Avoid duplicate use of the @dataclass decorator, which doesn't work
     if some options like frozen=True are used. *)
  if List.exists uses_dataclass_decorator decorators then
    decorators
  else
    decorators @ ["dataclass"]

let type_def env ((loc, (name, param, an), e) : A.type_def) : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  let class_decorators =
    get_class_decorators an
    |> List.map (fun s -> Line ("@" ^ s))
  in
  let rec unwrap e =
    match e with
    | Sum (loc, cases, an) ->
        sum env ~class_decorators loc name cases
    | Record (loc, fields, an) ->
        record env loc name fields an
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Name _ -> alias_wrapper env ~class_decorators name e
    | Shared _ -> not_implemented loc "cyclic references"
    | Wrap (loc, e, an) -> unwrap e
    | Tvar _ -> not_implemented loc "parametrized type"
  in
  unwrap e

let module_body env x =
  List.fold_left (fun acc (Type x) -> Inline (type_def env x) :: acc) [] x
  |> List.rev
  |> spaced

let definition_group ~atd_filename env
    (is_recursive, (items: A.module_body)) : B.t =
  [
    Inline (module_body env items);
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
let reserve_good_class_names env (items: A.module_body) =
  List.iter
    (fun (Type (loc, (name, param, an), e)) -> ignore (class_name env name))
    items

let to_file ~atd_filename ~head (items : A.module_body) dst_path =
  let env = init_env () in
  reserve_good_class_names env items;
  let head = List.map (fun s -> Line s) head in
  let python_defs =
    Atd.Util.tsort items
    |> List.map (fun x -> Inline (definition_group ~atd_filename env x))
  in
  Line (fixed_size_preamble atd_filename) :: Inline head :: python_defs
  |> double_spaced
  |> Indent.to_file ~indent:4 dst_path

let run_file src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ ".d"
    |> String.lowercase_ascii
  in
  let dst_path = dst_name in
  let full_module, _original_types =
    Atd.Util.load_file
      ~annot_schema
      ~expand:true (* monomorphization = eliminate parametrized type defs *)
      ~keep_builtins:true
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let full_module = Atd.Ast.use_only_specific_variants full_module in
  let (atd_head, atd_module) = full_module in
  let head = Dlang_annot.get_dlang_json_text (snd atd_head) in
  to_file ~atd_filename:src_name ~head atd_module dst_path
