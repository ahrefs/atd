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
  (* Local to a struct: instance variables, including method names *)
  translate_inst_variable: unit -> (string -> string);
}


let annot_schema_dlang : Atd.Annot.schema_section =
  {
    section = "dlang";
    fields = [
      Type_expr, "t";
      Type_expr, "repr";
      Type_expr, "unwrap";
      Type_expr, "wrap";
      Field, "default";
      Module_head, "import";
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

(* Use CamelCase *)
let struct_name env id =
  trans env (to_camel_case id)

(*
   Create a struct identifier that hasn't been seen yet.
   This is for internal disambiguation and still must translated using
   the 'trans' function ('struct_name' will not work due to trailing
   underscores being added for disambiguation).
*)
let create_struct_name env name =
  let preferred_id = to_camel_case name in
  env.create_variable preferred_id

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
        ["fromJson"; "toJson";
         "fromJsonString"; "toJsonString"]
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
  sprintf {|
// Generated by atdd from type definitions in %s.
// This implements classes for the types defined in '%s', providing
// methods and functions to convert data from/to JSON.

// ############################################################################
// # Private functions
// ############################################################################

module %s;

import std.algorithm : map;
import std.array : array;
import std.conv;
import std.format;
import std.json;
import std.sumtype;
import std.traits : isCallable, ReturnType;
import std.typecons : nullable, Nullable, tuple, Tuple;

private
{

  class AtdException : Exception
  {
      @safe this(string msg, string file = __FILE__, size_t line = __LINE__)
      {
          super(msg, file, line);
      }
  }

  // workaround to make toDelegate callable from safe
  @trusted auto toDelegate(F)(auto ref F fp) if (isCallable!F)
  {
    import std.functional;
    return std.functional.toDelegate(fp);
  }

  
  @trusted T _atd_missing_json_field(T)(string typeName, string jsonFieldName)
  {
      throw new AtdException("missing field %%s in JSON object of type %%s".format(jsonFieldName, typeName));
  }
  
  auto _atd_bad_json(T)(string expectedType, T jsonValue)
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
  
  auto _atd_bad_d(T)(string expectedType, T jsonValue)
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
  
  auto _atd_read_unit(JSONValue x)
  {
      if (x.isNull)
          return null;
      else
          throw _atd_bad_json("unit", x);
  }
  
  auto _atd_read_bool(JSONValue x)
  {
      try
          return x.boolean;
      catch (JSONException e)
          throw _atd_bad_json("bool", x);
  }
  
  auto _atd_read_int(JSONValue x)
  {
      try
          return cast(int) x.integer;
      catch (JSONException e)
          throw _atd_bad_json("int", x);
  }
  
  auto _atd_read_float(JSONValue x)
  {
      try
          return cast(float) x.floating;
      catch (JSONException e)
          throw _atd_bad_json("float", x);
  }
  
  auto _atd_read_string(JSONValue x)
  {
      try
          return x.str;
      catch (JSONException e)
          throw _atd_bad_json("string", x);
  }
  
  template _atd_read_list(alias readElements)
    {
        auto _atd_read_list(JSONValue jsonVal)
        {
            if (jsonVal.type != JSONType.array)
                throw _atd_bad_json("array", jsonVal);
            auto list = jsonVal.array;
            return array(list.map!readElements());
        }
    }

    template _atd_read_object_to_assoc_array(alias readValue)
    {
        auto _atd_read_object_to_assoc_array(JSONValue jsonVal)
        {
            alias T = ReturnType!readValue;

            if (jsonVal.type != JSONType.object)
                throw _atd_bad_json("object", jsonVal);
            T[string] ret;
            foreach (key, val; jsonVal.object)
                ret[key] = readValue(val);
            return ret;
        }
    }

    template _atd_read_array_to_assoc_dict(alias readKey, alias readValue)
    {
        auto _atd_read_array_to_assoc_dict(JSONValue jsonVal)
        {
            alias K = ReturnType!readKey;
            alias V = ReturnType!readValue;

            if (jsonVal.type != JSONType.array)
                throw _atd_bad_json("list", jsonVal);
            V[K] ret;
            foreach (jsonInnerVal; jsonVal.array)
            {
                if (jsonInnerVal.type != JSONType.array)
                    throw _atd_bad_json("list", jsonInnerVal);
                ret[readKey(jsonInnerVal[0])] = readValue(jsonInnerVal[1]);
            }
            return ret;
        }
    }

    template _atd_read_object_to_tuple_list(alias readValue)
    {
        auto _atd_read_object_to_tuple_list(JSONValue jsonVal)
        {
            alias T = ReturnType!readValue;

            if (jsonVal.type != JSONType.object)
                throw _atd_bad_json("object", jsonVal);
            auto tupList = new Tuple!(string, T)[](jsonVal.object.length);
            int i = 0;
            foreach (key, val; jsonVal.object)
                tupList[i++] = tuple(key, readValue(val));
            return tupList;
        }
    }

    template _atd_read_nullable(alias readElm)
    {
        auto _atd_read_nullable(JSONValue e)
        {
            alias T = ReturnType!readElm;

            if (e.isNull)
                return Nullable!T.init;
            else
                return Nullable!T(readElm(e));
        }
    }

    template _atd_read_option(alias readElm)
    {
        auto _atd_read_option(JSONValue e)
        {
            alias T = ReturnType!readElm;

            if (e.type == JSONType.string && e.str == "None")
                return Nullable!T.init;
            else if (e.type == JSONType.array && e.array.length == 2 && e[0].type == JSONType.string && e[0].str == "Some")
                return Nullable!T(readElm(e[1]));
            else
                throw _atd_bad_json("option", e);
        }
    }

    template _atd_read_wrap(alias readElm, alias wrap)
    {
        auto _atd_read_wrap(JSONValue e)
        {
            return wrap(readElm(e));
        }
    }

    // this whole set of function could be remplaced by one templated _atd_write_value function
    // not sure it is what we want though

    auto _atd_write_unit(typeof(null) n)
    {
        return JSONValue(null);
    }

    auto _atd_write_bool(bool b)
    {
        return JSONValue(b);
    }

    auto _atd_write_int(int i)
    {
        return JSONValue(i);
    }

    auto _atd_write_float(float f)
    {
        return JSONValue(f);
    }

    auto _atd_write_string(string s)
    {
        return JSONValue(s);
    }

    template _atd_write_list(alias writeElm)
    {
        auto _atd_write_list(T)(T[] list)
        {
            return JSONValue(array(list.map!writeElm()));
        }
    }

    template _atd_write_assoc_array_to_object(alias writeValue)
    {
        auto _atd_write_assoc_array_to_object(T)(T[string] assocArr)
        {
            JSONValue[string] ret;
            foreach (key, val; assocArr)
                ret[key] = writeValue(val);
            return JSONValue(ret);
        }
    }

    template _atd_write_assoc_dict_to_array(alias writeKey, alias writeValue)
    {
        auto _atd_write_assoc_dict_to_array(K, V)(V[K] assocArr)
        {
            JSONValue[] ret;
            foreach (key, val; assocArr)
                ret ~= JSONValue([writeKey(key), writeValue(val)]);
            return JSONValue(ret);
        }
    }

    template _atd_write_tuple_list_to_object(alias writeValue)
    {
        auto _atd_write_tuple_list_to_object(T)(Tuple!(string, T)[] tupList)
        {
            JSONValue[string] ret;
            foreach (tup; tupList)
                ret[tup[0]] = writeValue(tup[1]);
            return JSONValue(ret);
        }
    }

    template _atd_write_nullable(alias writeElm)
    {
        auto _atd_write_nullable(T)(Nullable!T elm)
        {
            if (elm.isNull)
                return JSONValue(null);
            else
                return writeElm(elm.get);
        }
    }

    template _atd_write_option(alias writeElm)
    {
        auto _atd_write_option(T)(Nullable!T elm)
        {
            if (elm.isNull)
                return JSONValue("None");
            else
                return JSONValue([JSONValue("Some"), writeElm(elm.get)]);
        }
    }

    template _atd_write_wrap(alias writeElm, alias unwrap)
    {
        auto _atd_write_wrap(Wrapped)(Wrapped e)
        {
            return writeElm(unwrap(e));
        }
    }
}

// ############################################################################
// # Public classes
// ############################################################################

auto fromJsonString(T)(string s)
{
    JSONValue res = parseJSON(s);
    return res.fromJson!T;
}

auto toJsonString(T)(T obj)
{
    JSONValue res = obj.toJson!T;
    return res.toString;
}

  |}
    atd_filename
    atd_filename
    (sprintf "%s_atd" (Filename.remove_extension atd_filename))


let not_implemented loc msg =
  A.error_at loc ("not implemented in atdd: " ^ msg)

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
  | "abstract" -> "JSONValue"
  | user_defined -> 
      let typename = (struct_name env user_defined) in
      typename

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
             (type_name_of_expr env value)
             (type_name_of_expr env key)
       | Object_dict value ->
           sprintf "%s[string]"
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "Nullable!%s" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "Nullable!%s" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared" (* TODO *)
  | Wrap (loc, e, an) ->
      (match Dlang_annot.get_dlang_wrap loc an with
       | None -> error_at loc "wrap type declared, but no dlang annotation found"
       | Some { dlang_wrap_t ; _ } -> dlang_wrap_t
      )
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
  | Nullable _ -> None
  | Shared (loc, e, an) -> get_default_default e
  | Wrap (loc, e, an) -> get_default_default e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "unit" -> None
       | "bool" -> Some "false"
       | "int" -> Some "0"
       | "float" -> Some "0.0"
       | "string" -> Some {|""|}
       | "abstract" -> None
       | _ -> None
      )
  | Name _ -> None
  | Tvar _ -> None

let get_dlang_default (e : type_expr) (an : annot) : string option =
  let user_default = Dlang_annot.get_dlang_default an in
  match user_default with
  | Some s -> Some s
  | None -> get_default_default e

(* If the field is '?foo: bar option', its dlang or json value has type
   'bar' rather than 'bar option'. *)
let unwrap_field_type loc field_name kind e = (* todo : dubious for dlang*)
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

let rec json_writer ?(nested=false) env e =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, cells, an) -> tuple_writer env (loc, cells, an)
  | List (loc, e, an) ->
      (match assoc_kind loc e an with
       | Array_list ->
           sprintf "_atd_write_list!(%s)" (json_writer ~nested:true env e)
       | Array_dict (key, value) ->
           sprintf "_atd_write_assoc_dict_to_array!(%s, %s)"
             (json_writer ~nested:true env key) (json_writer ~nested:true env value)
       | Object_dict value ->
           sprintf "_atd_write_assoc_array_to_object!(%s)"
             (json_writer ~nested:true env value)
       | Object_list value ->
           sprintf "_atd_write_tuple_list_to_object!(%s)"
             (json_writer ~nested:true env value)
      )
  | Option (loc, e, an) ->
      sprintf "_atd_write_option!(%s)"(json_writer ~nested:true env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_write_nullable!(%s)" (json_writer ~nested:true env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> 
    (match Dlang_annot.get_dlang_wrap loc an with
   | None -> error_at loc "wrap type declared, but no dlang annotation found"
   | Some { dlang_wrap_t; dlang_unwrap ; _ } ->
      sprintf "_atd_write_wrap!(%s, (%s e) => %s(e))" (json_writer ~nested:true env e) dlang_wrap_t dlang_unwrap
    ) 
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" -> sprintf "_atd_write_%s" name
       | "abstract" -> "(JSONValue x) => x"
       | _ -> let dtype_name = (dlang_type_name env name) in
          sprintf "((%s x) => x.toJson!(%s))" dtype_name dtype_name)
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

and tuple_writer env (loc, cells, an) =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_writer env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "((%s x) => JSONValue([%s]))"
    (type_name_of_expr env (Tuple (loc, cells, an)))
    tuple_body

let construct_json_field env trans_meth
    ((loc, (name, kind, an), e) : simple_field) =
  let unwrapped_type = unwrap_field_type loc name kind e in
  let writer_function = json_writer env unwrapped_type in
  let assignment =
    [
      Line (sprintf "res[\"%s\"] = %s(obj.%s);"
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
        Line (sprintf "if (!obj.%s.isNull)"
                (inst_var_name trans_meth name));
     Block [ Line(sprintf "res[\"%s\"] = %s(%s)(obj.%s);"
              (Atd.Json.get_json_fname name an |> single_esc)
              "_atd_write_option!"
              (json_writer ~nested:true env unwrapped_type)
              (inst_var_name trans_meth name))];
      ]

(*
   Function value that can be applied to a JSON node, converting it
   to the desired value.
*)
let rec json_reader ?(nested=false) env (e : type_expr) =
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
           sprintf "_atd_read_list!(%s)"
             (json_reader ~nested:true env e)
       | Array_dict (key, value) ->
           sprintf "_atd_read_array_to_assoc_dict!(%s, %s)"
             (json_reader ~nested:true env key) (json_reader ~nested:true env value)
       | Object_dict value ->
           sprintf "_atd_read_object_to_assoc_array!(%s)"
             (json_reader ~nested:true env value)
       | Object_list value ->
           sprintf "_atd_read_object_to_tuple_list!(%s)"
             (json_reader ~nested:true env value)
      )
  | Option (loc, e, an) ->
      sprintf "_atd_read_option!(%s)" (json_reader ~nested:true env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_read_nullable!(%s)" (json_reader ~nested:true env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) ->
    (match Dlang_annot.get_dlang_wrap loc an with
   | None -> error_at loc "wrap type declared, but no dlang annotation found"
   | Some { dlang_wrap ; _ } ->
      sprintf "_atd_read_wrap!(%s, (%s e) => %s(e))" (json_reader ~nested:true env e) (type_name_of_expr env e) dlang_wrap
    )
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" -> sprintf "_atd_read_%s" name
       | "abstract" -> "((JSONValue x) => x)"
       | _ -> sprintf "fromJson!%s" 
       (struct_name env name)
       )
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

and tuple_reader env cells =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%s(x[%i])" (json_reader env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "((JSONValue x) @trusted { 
    if (x.type != JSONType.array || x.array.length != %d)
      throw _atd_bad_json(\"Tuple of size %d\", x);
    return tuple(%s);
  })" (List.length cells) (List.length cells) tuple_body

let from_json_class_argument
    env trans_meth dlang_struct_name ((loc, (name, kind, an), e) : simple_field) =
  let dlang_name = inst_var_name trans_meth name in
  let json_name = Atd.Json.get_json_fname name an in
  let else_body =
    match kind with
    | Required ->
        sprintf "_atd_missing_json_field!(typeof(obj.%s))(\"%s\", \"%s\")"
          dlang_name
          (single_esc dlang_struct_name)
          (single_esc json_name)
    | Optional -> (sprintf "typeof(obj.%s).init" dlang_name)
    | With_default ->
        match get_dlang_default e an with
        | Some x -> x
        | None ->
            A.error_at loc
              (sprintf "missing default Dlang value for field '%s'"
                 name)
  in
  sprintf "obj.%s = (\"%s\" in x) ? %s(x[\"%s\"]) : %s;"
    dlang_name
    (single_esc json_name)
    (json_reader env e)
    (single_esc json_name)
    else_body

let inst_var_declaration
    env trans_meth ((loc, (name, kind, an), e) : simple_field) =
  let var_name = inst_var_name trans_meth name in
  let type_name = type_name_of_expr env e in
  let unwrapped_e = unwrap_field_type loc name kind e in
  let default =
    match kind with
    | Required
    | Optional -> ""
    | With_default ->
        match get_dlang_default unwrapped_e an with
        | None -> ""
        | Some x -> sprintf " = %s" x
  in
  [
    Line (sprintf "%s %s%s;" type_name var_name default)
  ]

let record env loc name (fields : field list) an =
  let dlang_struct_name = struct_name env name in
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
      Line (sprintf "@trusted %s fromJson(T : %s)(JSONValue x) {"
            (single_esc dlang_struct_name) (single_esc dlang_struct_name));
      Block [
        Line (sprintf "%s obj;" dlang_struct_name);
        Inline from_json_class_arguments;
        Line "return obj;";
      ];
      Line "}";
    ]
  in
  let to_json =
    [
      Line (sprintf "@trusted JSONValue toJson(T : %s)(T obj) {" (single_esc dlang_struct_name));
      Block [
        Line ("JSONValue res;");
        Inline json_object_body;
        Line "return res;"
      ];
      Line "}";
    ]
  in
  [
    Line (sprintf "struct %s {" dlang_struct_name);
    Block (spaced [
      Inline inst_var_declarations;
    ]);
    Line ("}");
    Line "";
    Inline from_json;
    Inline to_json;
  ]

let alias_wrapper env  name type_expr =
  let dlang_struct_name = struct_name env name in
  let value_type = type_name_of_expr env type_expr in
  let optional_constructor = match type_expr with
    | Tuple (_, _, _) -> 
      Line(sprintf "this(T...)(T args) @safe {_data = tuple(args);}");
    | _ -> Line(""); in
  [
    Line (sprintf "struct %s{%s _data; alias _data this;" dlang_struct_name value_type); 
    Line (sprintf "this(%s init) @safe {_data = init;}" value_type );
    Line (sprintf "this(%s init) @safe {_data = init._data;}" dlang_struct_name);
    Inline [optional_constructor];
    Line ("}");
    Line (sprintf "@trusted JSONValue toJson(T : %s)(%s e) {"  dlang_struct_name dlang_struct_name);
    Block [Line(sprintf "return %s(e);" (json_writer env type_expr))];
    Line("}");
    Line (sprintf "@trusted %s fromJson(T : %s)(JSONValue e) {" dlang_struct_name dlang_struct_name);
    Block [Line(sprintf "return %s(%s(e));" dlang_struct_name (json_reader env type_expr))];
    Line("}");
  ]

let case_class env  type_name
    (loc, orig_name, unique_name, an, opt_e) =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match opt_e with
  | None ->
      [
          Line (sprintf {|// Original type: %s = [ ... | %s | ... ]|}
                  type_name
                  orig_name);
          Line (sprintf "struct %s {}" (trans env unique_name));
          Line (sprintf "@trusted JSONValue toJson(T : %s)(T e) {"  (trans env unique_name));
          Block [Line(sprintf "return JSONValue(\"%s\");" (single_esc json_name))];
          Line("}");
        ]
  | Some e ->
      [
          Line (sprintf {|// Original type: %s = [ ... | %s of ... | ... ]|}
                  type_name
                  orig_name);
          Line (sprintf "struct %s { %s value; }" (trans env unique_name) (type_name_of_expr env e)); (* TODO : very dubious*)
          Line (sprintf "@trusted JSONValue toJson(T : %s)(T e) {"  (trans env unique_name));
          Block [Line(sprintf "return JSONValue([JSONValue(\"%s\"), %s(e.value)]);" (single_esc json_name) (json_writer env e))];
          Line("}");
        ]
      

let read_cases0 env loc name cases0 =
  let ifs =
    cases0
    |> List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
      let json_name = Atd.Json.get_json_cons orig_name an in
      Inline [
        Line (sprintf "if (x.str == \"%s\") " (single_esc json_name));
        Block [
          Line (sprintf "return %s(%s());" (struct_name env name) (trans env unique_name))
        ];
      ]
    )
  in
  [
    Inline ifs;
    Line (sprintf "throw _atd_bad_json(\"%s\", x);"
            (struct_name env name |> single_esc))
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
        Line (sprintf "if (cons == \"%s\")" (single_esc json_name));
        Block [
          Line (sprintf "return %s(%s(%s(x[1])));"
                  (struct_name env name)
                  (trans env unique_name)
                  (json_reader env e))
        ]
      ]
    )
  in
  [
    Inline ifs;
    Line (sprintf "throw _atd_bad_json(\"%s\", x);"
            (struct_name env name |> single_esc))
  ]

let sum_container env  loc name cases =
  let dlang_struct_name = struct_name env name in
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
        Line "if (x.type == JSONType.string) {";
        Block (read_cases0 env loc name cases0);
        Line "}";
      ]
    else
      []
  in
  let cases1_block =
    if cases1 <> [] then
      [
        Line "if (x.type == JSONType.array && x.array.length == 2 && x[0].type == JSONType.string) {";
        Block [
          Line "string cons = x[0].str;";
          Inline (read_cases1 env loc name cases1)
        ];
          Line "}";
      ]
    else
      []
  in
  [
    Line (sprintf "struct %s{ %s _data; alias _data this;" dlang_struct_name (sprintf "SumType!(%s)" type_list) ); 
    Line (sprintf "@safe this(T)(T init) {_data = init;} @safe this(%s init) {_data = init._data;}}" dlang_struct_name);
    Line "";
      Line (sprintf "@trusted %s fromJson(T : %s)(JSONValue x) {"
            (single_esc dlang_struct_name) (single_esc dlang_struct_name));
      Block [
        Inline cases0_block;
        Inline cases1_block;
        Line (sprintf "throw _atd_bad_json(\"%s\", x);"
                (single_esc (struct_name env name)))
      ];
    Line "}";
    Line "";
    Line (sprintf "@trusted JSONValue toJson(T : %s)(T x) {" (dlang_struct_name));
    Block [
      Line "return x.match!(";
        Line (
                 List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
                   sprintf "(%s v) => v.toJson!(%s)" (trans env unique_name) (trans env unique_name)
                 ) cases
              |> String.concat ",\n");
        Line ");"
    ];
      Line "}";
  ]


let sum env  loc name cases =
  let cases =
    List.map (fun (x : variant) ->
      match x with
      | Variant (loc, (orig_name, an), opt_e) ->
          let unique_name = create_struct_name env orig_name in
          (loc, orig_name, unique_name, an, opt_e)
      | Inherit _ -> assert false
    ) cases
  in
  let case_classes =
    List.map (fun x -> Inline (case_class env name x)) cases
    |> double_spaced
  in
  let container_class = sum_container env loc name cases in
  [
    Inline case_classes;
    Inline container_class;
  ]
  |> double_spaced

let type_def env ((loc, (name, param, an), e) : A.type_def) : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  let unwrap e =
    match e with
    | Sum (loc, cases, an) ->
        sum env  loc name cases
    | Record (loc, fields, an) ->
        record env loc name fields an
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Wrap _
    | Name _ -> alias_wrapper env  name e
    | Shared _ -> not_implemented loc "cyclic references"
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
   For example, type 'foo' should become struct 'Foo'.
   We do this because each case constructor of sum types will also
   translate to a struct in the same namespace. For example,
   there may be a type like 'type bar = [ Foo | Bleep ]'.
   We want to ensure that the type 'foo' gets the name 'Foo' and that only
   later the case 'Foo' gets a lesser name like 'Foo_' or 'Foo2'.
*)
let reserve_good_struct_names env (items: A.module_body) =
  List.iter
    (fun (Type (loc, (name, param, an), e)) -> ignore (struct_name env name))
    items

let to_file ~atd_filename ~head (items : A.module_body) dst_path =
  let env = init_env () in
  reserve_good_struct_names env items;
  let head = List.map (fun s -> Line s) head in
  let dlang_defs =
    Atd.Util.tsort items
    |> List.map (fun x -> Inline (definition_group ~atd_filename env x))
  in
  Line (fixed_size_preamble atd_filename) :: Inline head :: dlang_defs
  |> double_spaced
  |> Indent.to_file ~indent:4 dst_path

let run_file ~tags src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ "_atd.d"
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
      ~tags
      src_path
  in
  let full_module = Atd.Ast.use_only_specific_variants full_module in
  let (atd_head, atd_module) = full_module in
  let head =
     Dlang_annot.get_dlang_import (snd atd_head) 
    |> List.map (sprintf "import %s;")
  in
  to_file ~atd_filename:src_name ~head atd_module dst_path
