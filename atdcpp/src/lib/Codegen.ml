(*
   cpp code generation for JSON support (no biniou support)

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


let annot_schema_cpp : Atd.Annot.schema_section =
  {
    section = "cpp";
    fields = [
      Type_expr, "t";
      Type_expr, "repr";
      Type_expr, "unwrap";
      Type_expr, "wrap";
      Type_expr, "templatize";
      Field, "default";
      Module_head, "include";
      Module_head, "namespace";
    ]
  }

let annot_schema : Atd.Annot.schema =
  annot_schema_cpp :: Atd.Json.annot_schema_json

(* Translate a preferred variable name into an available cpp identifier. *)
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
       https://cpp.org/spec/lex.html#keywords
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

let fixed_size_preamble_header atd_filename = 
 sprintf {|
// Generated by atdcpp from type definitions in %s.
// This implements classes for the types defined in '%s', providing
// methods and functions to convert data from/to JSON.

// ############################################################################

#pragma once

#include <stdexcept>
#include <string>
#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
#include <variant>
#include <memory>
#include <optional>

|} atd_filename atd_filename

let fixed_size_preamble atd_filename namespace_name =
  sprintf {|
// Generated by atdcpp from type definitions in %s.
// This implements classes for the types defined in '%s', providing
// methods and functions to convert data from/to JSON.

// ############################################################################
// # Private functions
// ############################################################################

// filename %s

#include "%s_atd.hpp"

namespace %s
{

namespace // anonymous
{

std::string _rapid_json_type_to_string(rapidjson::Type type)
{
    switch (type)
    {
    case rapidjson::kNullType:
        return "null";
    case rapidjson::kFalseType:
        return "false";
    case rapidjson::kTrueType:
        return "true";
    case rapidjson::kObjectType:
        return "object";
    case rapidjson::kArrayType:
        return "array";
    case rapidjson::kStringType:
        return "string";
    case rapidjson::kNumberType:
        return "number";
    default:
        return "unknown";
    }
}

class AtdException : public std::exception
{
public:
    AtdException(const std::string &message) : msg_(message) {}

    const char *what() const throw() override
    {
        return msg_.c_str();
    }

private:
    std::string msg_;
};

template <typename T>
[[maybe_unused]] T _atd_missing_json_field(const std::string &type, const std::string &field)
{
    throw AtdException("Missing JSON field '" + field + "' in " + type);
}

[[maybe_unused]] auto _atd_bad_json(const std::string &type, const rapidjson::Value &x)
{
    auto x_type = x.GetType();
    return AtdException("Bad JSON for " + type + " got type " + _rapid_json_type_to_string(x_type));
}

// Reading an integer from JSON
[[maybe_unused]] int _atd_read_int(const rapidjson::Value &val)
{
    if (!val.IsInt())
    {
        throw AtdException("Expected an integer but got" + _rapid_json_type_to_string(val.GetType()));
    }
    return val.GetInt();
}

[[maybe_unused]] bool _atd_read_bool(const rapidjson::Value &val)
{
    if (!val.IsBool())
    {
        throw AtdException("Expected a boolean but got" + _rapid_json_type_to_string(val.GetType()));
    }
    return val.GetBool();
}

// Reading a float from JSON
[[maybe_unused]] double _atd_read_float(const rapidjson::Value &val)
{
    if (!val.IsNumber())
    {
        throw AtdException("Expected a number but got" + _rapid_json_type_to_string(val.GetType()));
    }

    return val.GetDouble();
}

[[maybe_unused]] std::string _atd_read_string(const rapidjson::Value &val)
{
    if (!val.IsString())
    {
        throw AtdException("Expected a string but got" + _rapid_json_type_to_string(val.GetType()));
    }
    return val.GetString();
}

[[maybe_unused]] std::string _atd_read_abstract(const rapidjson::Value &val)
{
  // will convert val to string
  rapidjson::StringBuffer buffer;
  rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
  val.Accept(writer);
  return buffer.GetString();
}

template <typename F>
[[maybe_unused]] auto _atd_read_array(F read_func, const rapidjson::Value &val)
{
    using ResultType = typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type;

    if (!val.IsArray())
    {
        throw AtdException("Expected an array but got" + _rapid_json_type_to_string(val.GetType()));
    }

    std::vector<ResultType> result;
    for (rapidjson::SizeType i = 0; i < val.Size(); i++)
    {
        result.push_back(read_func(val[i]));
    }

    return result;
}

template<typename F>
[[maybe_unused]] auto _atd_read_object_to_tuple_list(F read_func, const rapidjson::Value &val)
{
    using ResultType = typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type;

    if (!val.IsObject())
    {
        throw AtdException("Expected an object but got" + _rapid_json_type_to_string(val.GetType()));
    }

    std::vector<std::tuple<std::string, ResultType>> result;
    for (auto &m : val.GetObject())
    {
        result.push_back(std::make_tuple(m.name.GetString(), read_func(m.value)));
    }

    return result;
}

template<typename RK, typename RV>
[[maybe_unused]] auto _atd_read_array_to_assoc_dict(RK read_key_func, RV read_value_func, const rapidjson::Value &val)
{
    using KeyType = typename std::invoke_result<decltype(read_key_func), const rapidjson::Value &>::type;
    using ValueType = typename std::invoke_result<decltype(read_value_func), const rapidjson::Value &>::type;

    if (!val.IsArray())
    {
        throw AtdException("Expected an array but got" + _rapid_json_type_to_string(val.GetType()));
    }

    std::map<KeyType, ValueType> result;
    for (rapidjson::SizeType i = 0; i < val.Size(); i++)
    {
        auto &pair = val[i];
        if (!pair.IsArray() || pair.Size() != 2)
        {
            throw AtdException("Expected an array of pairs");
        }
        result[read_key_func(pair[0])] = read_value_func(pair[1]);
    }

    return result;
}

template<typename F>
[[maybe_unused]] auto _atd_read_object_to_assoc_array(F read_func, const rapidjson::Value &val)
{
    using ResultType = typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type;

    if (!val.IsObject())
    {
        throw AtdException("Expected an object but got" + _rapid_json_type_to_string(val.GetType()));
    }

    std::map<std::string, ResultType> result;
    for (auto &m : val.GetObject())
    {
        result[m.name.GetString()] = read_func(m.value);
    }

    return result;
}

template<typename F>
[[maybe_unused]] auto _atd_read_nullable(F read_func, const rapidjson::Value &val)
{
    if (val.IsNull())
    {
        return std::optional<typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type>();
    }
    return std::optional<typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type>(read_func(val));
}

template<typename F>
[[maybe_unused]] auto _atd_read_option(F read_func, const rapidjson::Value &val)
{
    using ResultType = typename std::invoke_result<decltype(read_func), const rapidjson::Value &>::type;
    if (val.IsString() && std::string_view(val.GetString()) == "None")
    {
        return std::optional<ResultType>();
    }
    else if (val.IsArray() && val.Size() == 2 && val[0].IsString() && std::string_view(val[0].GetString()) == "Some")
    {
        return std::make_optional(read_func(val[1]));
    }
    else
    {
        throw AtdException("Expected an option");
    }
}

template <typename F, typename W>
[[maybe_unused]] auto _atd_read_wrap(F read_func, W wrap_func, const rapidjson::Value &val)
{
    return wrap_func(read_func(val));
}

template <typename T>
[[maybe_unused]] std::shared_ptr<T> _atd_wrap_shared_ptr(T val)
{
    return std::make_shared<T>(val);
}

template <typename T>
[[maybe_unused]] T _atd_unwrap_shared_ptr(const std::shared_ptr<T> val)
{
    if (!val)
    {
        throw AtdException("Expected a shared_ptr but got nullptr");
    }
    return *val;
}

[[maybe_unused]] void _atd_write_int(int value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.Int(value);
}

[[maybe_unused]] void _atd_write_bool(bool value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.Bool(value);
}

[[maybe_unused]] void _atd_write_float(double value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.Double(value);
}

[[maybe_unused]] void _atd_write_string(const std::string &value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.String(value.c_str());
}

[[maybe_unused]] void _atd_write_abstract(const std::string &value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    // writes string value as raw json
    writer.RawValue(value.c_str(), value.size(), rapidjson::kStringType);
}

template <typename F, typename V>
[[maybe_unused]] void _atd_write_array(F write_func, const V& values, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.StartArray();
    for (const auto& value : values)
    {
        write_func(value, writer);
    }
    writer.EndArray();
}

template <typename F, typename V>
[[maybe_unused]] void _atd_write_tuple_list_to_object(F write_func, const V &values, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.StartObject();
    for (const auto& value : values)
    {
        writer.Key(std::get<0>(value).c_str());
        write_func(std::get<1>(value), writer);
    }
    writer.EndObject();
}

template <typename Wk, typename Wv, typename Map>
[[maybe_unused]] void _atd_write_assoc_dict_to_array(const Wk write_key_func, const Wv write_value_func, const Map &value_map, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.StartArray();
    for (const auto& pair : value_map)
    {
        writer.StartArray();
        write_key_func(pair.first, writer);
        write_value_func(pair.second, writer);
        writer.EndArray();
    }
    writer.EndArray();
}

template <typename F, typename Map>
[[maybe_unused]] void _atd_write_assoc_array_to_object(F write_func, const Map &value_map, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.StartObject();
    for (const auto& pair : value_map)
    {
        writer.Key(pair.first.c_str());
        write_func(pair.second, writer);
    }
    writer.EndObject();
}


template <typename F, typename O>
[[maybe_unused]] void _atd_write_option(F write_func, const O &val, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    if (val)
    {
        writer.StartArray();
        writer.String("Some");
        write_func(*val, writer);
        writer.EndArray();
    }
    else
    {
        writer.String("None");
    }
}

template <typename F, typename O>
[[maybe_unused]] void _atd_write_nullable(F write_func, const O &val, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    if (val)
    {
        write_func(*val, writer);
    }
    else
    {
        writer.Null();
    }
}

template <typename F, typename W, typename T>
[[maybe_unused]] void _atd_write_wrap(F write_func, W wrap_func, const T &val, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    write_func(wrap_func(val), writer);
}
} // anonymous namespace
  |}
    atd_filename
    atd_filename
    atd_filename
    (Filename.chop_extension atd_filename)
    namespace_name


let not_implemented loc msg =
  A.error_at loc ("not implemented in atdcpp: " ^ msg)

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
   Representations of ATD type '(string * value) list' in JSON and cpp.
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
  let cpp_repr = Cpp_annot.get_cpp_assoc_repr an in
  match e, json_repr, cpp_repr with
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

(* Map ATD built-in types to built-in cpp types *)
let cpp_type_name env (name : string) =
  match name with
  | "unit" -> "void"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "double"
  | "string" -> "std::string"
  | "abstract" -> "std::string"
  | user_defined -> 
      let typename = (struct_name env user_defined) in
      typename

let cpp_type_name_namespaced env (name : string) = 
  match name with
  | "unit" -> "void"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "double"
  | "string" -> "std::string"
  | "abstract" -> "std::string"
  | user_defined -> 
      let typename = (struct_name env user_defined) in
      sprintf "%s::%s" "typedefs" typename            

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, xs, an) ->
      let type_names =
        xs
        |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
      in
      sprintf "std::tuple<%s>" (String.concat ", " type_names)
  | List (loc, e, an) ->
     (match assoc_kind loc e an with
       | Array_list
       | Object_list _ ->
           sprintf "std::vector<%s>"
             (type_name_of_expr env e)
       | Array_dict (key, value) ->
           sprintf "std::map<%s, %s>"
             (type_name_of_expr env key)
             (type_name_of_expr env value)
       | Object_dict value ->
           sprintf "std::map<std::string, %s>"
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "std::optional<%s>" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "std::optional<%s>" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared" (* TODO *)
  | Wrap (loc, e, an) ->
      (match Cpp_annot.get_cpp_wrap loc an with
       | None -> error_at loc "wrap type declared, but no cpp annotation found"
       | Some { cpp_wrap_t ; cpp_templatize=false; _ } -> cpp_wrap_t
       | Some { cpp_wrap_t ; cpp_templatize=true; _ } -> sprintf "%s<%s>" cpp_wrap_t (type_name_of_expr env e)        
      )
  | Name (loc, (loc2, name, []), an) -> cpp_type_name_namespaced env name
  | Name (loc, (_, name, _::_), _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

let rec get_default_default (e : type_expr) : string option =
  match e with
  | Sum _
  | Record _
  | Tuple _ (* a default tuple could be possible but we're lazy *) -> None
  | List _ -> Some "{}"
  | Option _
  | Nullable _ -> Some "std::nullopt"
  | Shared (loc, e, an) -> get_default_default e
  | Wrap (loc, e, an) -> get_default_default e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "unit" -> None
       | "bool" -> Some "false"
       | "int" -> Some "0"
       | "float" -> Some "0.0f"
       | "string" -> Some {|""|}
       | "abstract" -> None
       | _ -> None
      )
  | Name _ -> None
  | Tvar _ -> None


type cpp_default = | User_default of string | Default_default of string | No_default 

let get_cpp_default (e : type_expr) (an : annot) : cpp_default =
  let user_default = Cpp_annot.get_cpp_default an in
  match user_default with
  | Some s -> User_default s
  | None -> (match get_default_default e with 
    | Some s -> Default_default s 
    | None -> No_default)

(* If the field is '?foo: bar option', its cpp or json value has type
   'bar' rather than 'bar option'. *)
let unwrap_field_type loc field_name kind e = (* todo : dubious for cpp*)
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
           sprintf "_atd_write_array([](auto v, auto &w){%sv, w);}, " (json_writer ~nested:true env e)
       | Array_dict (key, value) ->
           sprintf "_atd_write_assoc_dict_to_array([](auto v, auto &w){%sv, w);}, [](auto v, auto &w){%sv, w);}, "
             (json_writer ~nested:true env key) (json_writer ~nested:true env value)
       | Object_dict value ->
           sprintf "_atd_write_assoc_array_to_object([](auto v, auto &w){%sv, w);}, "
             (json_writer ~nested:true env value)
       | Object_list value ->
           sprintf "_atd_write_tuple_list_to_object([](auto v, auto &w){%sv, w);}, "
             (json_writer ~nested:true env value)
      )
  | Option (loc, e, an) ->
      sprintf "_atd_write_option([](auto v, auto &w){%sv, w);}, "(json_writer ~nested:true env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_write_nullable([](auto v, auto &w){%sv, w);}, " (json_writer ~nested:true env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> 
    (match Cpp_annot.get_cpp_wrap loc an with
   | None -> error_at loc "wrap type declared, but no cpp annotation found"
   | Some { cpp_wrap_t; cpp_unwrap ; _ } ->
      sprintf "_atd_write_wrap([](const auto &v, auto &w){%sv, w);}, [](const auto &e){return %s(e);}, " (json_writer ~nested:true env e) cpp_unwrap
    ) 
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" | "abstract" -> sprintf "_atd_write_%s(" name
       | _ -> let dtype_name = (cpp_type_name env name) in
          sprintf "%s::to_json(" dtype_name)
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

and tuple_writer env (loc, cells, an) =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%sstd::get<%i>(t), writer)" (json_writer env e) i
    ) cells
    |> String.concat "; "
  in
  sprintf "[](const auto &t, auto &writer){
    writer.StartArray();
      %s;
      writer.EndArray();
      }("
    tuple_body

let get_is_set_to_default env an e field =
  let cpp_default = get_cpp_default e an in
  let cpp_default_str = match cpp_default with | User_default x | Default_default x -> x | No_default -> "" in
  let default_format = sprintf "%s != %s(%s)" field (type_name_of_expr env e) cpp_default_str in
  match e with
  | List _ -> (match cpp_default with
    | User_default _ -> default_format
    | Default_default _ | No_default ->  sprintf "!%s.empty()" field
    )
  | _ -> default_format

let construct_json_field env trans_meth
    ((loc, (name, kind, an), e) : simple_field) =
  let unwrapped_type = unwrap_field_type loc name kind e in
  let writer_function n = json_writer ~nested:n env unwrapped_type in
  let cpp_var_name = inst_var_name trans_meth name in
  let assignement =
    [
      Line (sprintf "writer.Key(\"%s\");"
              (Atd.Json.get_json_fname name an |> single_esc));
      Line (sprintf "%st.%s, writer);" (writer_function false) (inst_var_name trans_meth name));
    ]
  in
  match kind with
  | Required -> assignement
  | With_default -> 
      [
        Line (sprintf "if (%s) {" (get_is_set_to_default env an e (sprintf "t.%s" cpp_var_name)));
        Block assignement; 
        Line "}"
      ]
  | Optional ->
      [
        Line (sprintf "if (t.%s != std::nullopt) {" cpp_var_name);
        Block [ 
          Line (sprintf "writer.Key(\"%s\");"
                    (Atd.Json.get_json_fname name an |> single_esc)); 
          Line (sprintf "%st.%s.value(), writer);" (writer_function true) cpp_var_name)
        ];
        Line "}";
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
           sprintf "_atd_read_array([](const auto &v){return %sv);}, " 
             (json_reader ~nested:true env e)
       | Array_dict (key, value) ->
           sprintf "_atd_read_array_to_assoc_dict([](const auto &k){return %sk);}, [](const auto &v){return %sv);}, "
             (json_reader ~nested:true env key) (json_reader ~nested:true env value)
       | Object_dict value ->
           sprintf "_atd_read_object_to_assoc_array([](const auto &v){return %sv);},"
             (json_reader ~nested:true env value)
       | Object_list value ->
           sprintf "_atd_read_object_to_tuple_list([](const auto &v){return %sv);},"
             (json_reader ~nested:true env value)
      )
  | Option (loc, e, an) ->
      sprintf "_atd_read_option([](const auto &v){return %sv);}, " (json_reader ~nested:true env e)
  | Nullable (loc, e, an) ->
      sprintf "_atd_read_nullable([](const auto &v){return %sv);}, " (json_reader ~nested:true env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) ->
    (match Cpp_annot.get_cpp_wrap loc an with
   | None -> error_at loc "wrap type declared, but no cpp annotation found"
   | Some { cpp_wrap ; _ } ->
      sprintf "_atd_read_wrap([](const auto& v){return %sv);}, [](const auto &e){return %s(e);}," (json_reader ~nested:true env e) cpp_wrap
    )
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "bool" | "int" | "float" | "string" | "abstract" -> sprintf "_atd_read_%s(" name
       | _ -> sprintf "%s::from_json(" 
       (struct_name env name)
       )
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

and tuple_reader env cells =
  let tuple_body =
    List.mapi (fun i (loc, e, an) ->
      sprintf "%sv[%i])" (json_reader env e) i
    ) cells
    |> String.concat ", "
  in
  sprintf "[](auto &v){
      if (!v.IsArray() || v.Size() != %d)
        throw AtdException(\"Tuple of size %d\");
      return std::make_tuple(%s);
      }("
    (List.length cells) (List.length cells) tuple_body

let from_json_class_argument
    env trans_meth cpp_struct_name ((loc, (name, kind, an), e) : simple_field) =
  let cpp_name = inst_var_name trans_meth name in
  let json_name = Atd.Json.get_json_fname name an in
  let else_body =
    match kind with
    | Required ->
        sprintf "_atd_missing_json_field<decltype(record.%s)>(\"%s\", \"%s\")"
          cpp_name
          (single_esc cpp_struct_name)
          (single_esc json_name)
    | Optional -> (sprintf "std::nullopt")
    | With_default ->
        match get_cpp_default e an with
        | User_default x | Default_default x -> x
        | No_default ->
            A.error_at loc
              (sprintf "missing default cpp value for field '%s'"
                 name)
  in
  let reader = 
    match kind with 
    | Optional -> (match e with 
      | Option(_, e, _) -> (json_reader env e) 
      | _ -> A.error_at loc "optional field must be of type 'xxx option'") 
    | _ -> (json_reader env e)
  in
  Inline [
    Line (sprintf "if (doc.HasMember(\"%s\"))" (single_esc json_name));
    Block [
      Line (sprintf "record.%s = %sdoc[\"%s\"]);"
              cpp_name
              reader
              (single_esc json_name));
    ];
    Line (sprintf "else record.%s = %s;" cpp_name else_body);]

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
        match get_cpp_default unwrapped_e an with
        | No_default -> ""
        | User_default x | Default_default x -> sprintf " = %s" x
  in
  [
    Line (sprintf "%s %s%s;" type_name var_name default)
  ]

let from_json_string_declaration cpp_name static = 
  let static_val = match static with | true -> "static " | false -> "" in
  [
    Line (sprintf "%s%s from_json_string(const std::string &s);" static_val cpp_name);
  ]

let from_json_string_definition cpp_name p = 
  [
    Line (sprintf "%s %sfrom_json_string(const std::string &s) {" cpp_name (match p with | Some x -> sprintf "%s::" x | None -> ""));
    Block [
      Line "rapidjson::Document doc;";
      Line "doc.Parse(s.c_str());";
      Line "if (doc.HasParseError()) {";
      Block [Line "throw AtdException(\"Failed to parse JSON\");"];
      Line "}";
      Line "return from_json(doc);";
    ];
    Line "}";
  ]

type codegen_type = 
  | Declaration 
  | Definition 
  | Forward_decl 
  | Struct_typedef
  | Alias_typedef
  | Variant_typedef

let record_definition env loc name fields an =
  let cpp_struct_name = struct_name env name in
  let trans_meth = env.translate_inst_variable () in
  let from_json_class_arguments =
    List.map (fun x ->
      (from_json_class_argument env trans_meth cpp_struct_name x)
    ) fields in
  let json_object_body =
    List.map (fun x ->
      Inline (construct_json_field env trans_meth x)) fields in
  let from_json =
    [
      Line (sprintf "%s %s::from_json(const rapidjson::Value & doc) {"
           (single_esc cpp_struct_name) (single_esc cpp_struct_name));
      Block [
        Line (sprintf "%s record;" cpp_struct_name);
        Line "if (!doc.IsObject()) {";
        Block [
          Line (sprintf "throw AtdException(\"atdtype: %s, expected an object but got\" + _rapid_json_type_to_string(doc.GetType()));" name);
          ];
        Line "}";
        Inline from_json_class_arguments;
        Line "return record;";
      ];
      Line "}";
    ]
  in
  let to_json =
    [
      Line (sprintf "void %s::to_json(const %s &t, rapidjson::Writer<rapidjson::StringBuffer> &writer) {" (single_esc cpp_struct_name) (single_esc cpp_struct_name));
      Block [
        Line ("writer.StartObject();");
        Inline json_object_body;
        Line ("writer.EndObject();");
      ];
      Line "}";
    ]
  in
  let to_json_string_static = 
    [
      Line (sprintf "std::string %s::to_json_string(const %s &t) {" (single_esc cpp_struct_name) (single_esc cpp_struct_name));
      Block [
        Line ("rapidjson::StringBuffer buffer;");
        Line ("rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);");
        Line ("to_json(t, writer);");
        Line "return buffer.GetString();"
      ];
      Line "}";
    ]
  in
  let to_json_string = 
    [
      Line (sprintf "std::string %s::to_json_string() {" (single_esc cpp_struct_name));
      Block [
        Line ("return to_json_string(*this);");
      ];
      Line "}";
    ]
  in
  [
    Inline from_json;
    Inline (from_json_string_definition cpp_struct_name (Some cpp_struct_name));
    Inline to_json;
    Inline to_json_string_static;
    Inline to_json_string;
  ]


let record env loc name (fields : field list) an codegen_type =
  let cpp_struct_name = struct_name env name in
  let trans_meth = env.translate_inst_variable () in
  let fields_l =
    List.map (function
      | `Field x -> x
      | `Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  let inst_var_declarations =
    List.map (fun x -> Inline (inst_var_declaration env trans_meth x)) fields_l
  in
  let from_json =
    [
      Line (sprintf "static %s from_json(const rapidjson::Value & doc);"
           (single_esc cpp_struct_name));
    ]
  in
  let to_json =
    [
      Line (sprintf "static void to_json(const %s &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);" (single_esc cpp_struct_name));
    ]
  in
  let to_json_string_static = 
    [
      Line (sprintf "static std::string to_json_string(const %s &t);" (single_esc cpp_struct_name));
    ]
  in
  let to_json_string = 
    [
      Line (sprintf "std::string to_json_string();" );
    ]
  in
  match codegen_type with
  | Declaration ->
  [
    Line (sprintf "struct %s {" cpp_struct_name);
    Block ([
      Inline inst_var_declarations;
      Line ("");
      Inline from_json;
      Inline (from_json_string_declaration cpp_struct_name true);
      Inline to_json;
      Inline to_json_string_static;
      Inline to_json_string;
    ]);
    Line ("};");
  ]
  | Definition -> record_definition env loc name fields_l an
  | Forward_decl -> [Line (sprintf "struct %s;" cpp_struct_name)]
  | Struct_typedef -> [Line (sprintf "typedef %s %s;" cpp_struct_name cpp_struct_name)]
  | _ -> []

let alias_wrapper env  name type_expr codegen_type =
  let cpp_struct_name = struct_name env name in
  match codegen_type with
  | Declaration ->
    [
      Line (sprintf "namespace %s {" cpp_struct_name);
      Block [
        Line (sprintf "typedefs::%s from_json(const rapidjson::Value &doc);" cpp_struct_name);
        Inline (from_json_string_declaration (sprintf "typedefs::%s" cpp_struct_name) false);
        Line (sprintf "void to_json(const typedefs::%s &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);" cpp_struct_name);
        Line (sprintf "std::string to_json_string(const typedefs::%s &t);" cpp_struct_name);
      ];
      Line "}";
    ]
  | Definition -> 
    [
      Line (sprintf "namespace %s {" cpp_struct_name);
      Block [
      Line (sprintf "typedefs::%s from_json(const rapidjson::Value &doc) {" cpp_struct_name);
      Block [
        Line (sprintf "return %sdoc);" (json_reader env type_expr));
      ];
      Line "}";
      Inline (from_json_string_definition (sprintf "typedefs::%s" cpp_struct_name) None);
      Line (sprintf "void to_json(const typedefs::%s &t, rapidjson::Writer<rapidjson::StringBuffer> &writer) {" cpp_struct_name);
      Block [
        Line (sprintf "%st, writer);" (json_writer env type_expr));
      ];
      Line "}";
      Line (sprintf "std::string to_json_string(const typedefs::%s &t) {" cpp_struct_name);
      Block [
        Line ("rapidjson::StringBuffer buffer;");
        Line ("rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);");
        Line (sprintf "to_json(t, writer);");
        Line "return buffer.GetString();"
      ];
      Line "}"; ];
      Line "}";
    ]
  | Alias_typedef -> [Line (sprintf "typedef %s %s;" (type_name_of_expr env type_expr) cpp_struct_name)]
  | _ -> []


let case_class env  type_name (loc, orig_name, unique_name, an, opt_e) case_classes =
  let json_name = Atd.Json.get_json_cons orig_name an in
  match case_classes with
  | Declaration -> (match opt_e with
    | None ->
        [
            Line (sprintf {|// Original type: %s = [ ... | %s | ... ]|}
                    type_name
                    orig_name);
            Line (sprintf "struct %s {" (trans env orig_name));
            Block [Line (sprintf "static void to_json(const %s &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);" (trans env orig_name));];
            Line (sprintf "};");
        ]
    | Some e ->
        [
            Line (sprintf {|// Original type: %s = [ ... | %s of ... | ... ]|}
                    type_name
                    orig_name);
            Line (sprintf "struct %s" (trans env orig_name));
            Line(sprintf "{");
            Block [
              Line (sprintf "%s value;" (type_name_of_expr env e));
              Line (sprintf "static void to_json(const %s &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);" (trans env orig_name));
            ];
            Line(sprintf "};");
        ])
  | Definition -> (match opt_e with
    | None ->
        [
            Line (sprintf "void %s::to_json(const %s &e, rapidjson::Writer<rapidjson::StringBuffer> &writer){" (trans env orig_name) (trans env orig_name));
            Block [
              Line (sprintf "writer.String(\"%s\");" (single_esc json_name));
            ];
            Line (sprintf "};");
        ]
    | Some e ->
        [
            Line (sprintf "void %s::to_json(const %s &e, rapidjson::Writer<rapidjson::StringBuffer> &writer){" (trans env orig_name) (trans env orig_name));
            Block [
              Line (sprintf "writer.StartArray();");
              Line (sprintf "writer.String(\"%s\");" (single_esc json_name));
              Line (sprintf "%se.value, writer);" (json_writer env e));
              Line (sprintf "writer.EndArray();");
            ];
            Line("}");
        ])
  | _ -> []
  
      

let read_cases0 env loc name cases0 sum_repr =
  let ifs =
    cases0
    |> List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
      let json_name = Atd.Json.get_json_cons orig_name an in
      Inline [
        Line (sprintf "if (std::string_view(x.GetString()) == \"%s\") " (single_esc json_name));
        Block [
          Line (sprintf "return Types::%s%s;" (trans env orig_name) (match sum_repr with | Cpp_annot.Variant -> "()" | Cpp_annot.Enum -> ""))
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
          Line (sprintf "return Types::%s({%sx[1])});"
                  (trans env orig_name)
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

let sum_container env  loc name cases codegen_type =
  let cpp_struct_name = struct_name env name in
  let cases0, cases1 =
    List.partition (fun (loc, orig_name, unique_name, an, opt_e) ->
      opt_e = None
    ) cases
  in
  let cases0_block =
    if cases0 <> [] then
      [
        Line "if (x.IsString()) {";
        Block (read_cases0 env loc name cases0 Cpp_annot.Variant);
        Line "}";
      ]
    else
      []
  in
  let cases1_block =
    if cases1 <> [] then
      [
        Line "if (x.IsArray() && x.Size() == 2 && x[0].IsString()) {";
        Block [
          Line "std::string cons = x[0].GetString();";
          Inline (read_cases1 env loc name cases1)
        ];
          Line "}";
      ]
    else
      []
  in
  match codegen_type with
  | Declaration -> 
  [
    Line (sprintf "typedefs::%s from_json(const rapidjson::Value &x);" (cpp_struct_name));
    Inline (from_json_string_declaration (sprintf "typedefs::%s" cpp_struct_name) false);
    Line (sprintf "void to_json(const typedefs::%s &x, rapidjson::Writer<rapidjson::StringBuffer> &writer);" (cpp_struct_name));
    Line (sprintf "std::string to_json_string(const typedefs::%s &x);" (cpp_struct_name));
  ]
  | Definition ->
  [
    Line (sprintf "namespace %s {" (cpp_struct_name));
    Block [
      Line (sprintf "typedefs::%s from_json(const rapidjson::Value &x) {" (cpp_struct_name));
      Block [
        Inline cases0_block;
        Inline cases1_block;
        Line (sprintf "throw _atd_bad_json(\"%s\", x);"
                (single_esc (struct_name env name)))
      ];
      Line "}";
    ];
    Block [Inline (from_json_string_definition (sprintf "typedefs::%s" cpp_struct_name) (None))];
    Block [
      Line (sprintf "void to_json(const typedefs::%s &x, rapidjson::Writer<rapidjson::StringBuffer> &writer) {" (cpp_struct_name));
      Block [
        Line "std::visit([&writer](auto &&arg) {";
        Block [
          Line "using T = std::decay_t<decltype(arg)>;";
          Block (
            List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
              Line (sprintf "if constexpr (std::is_same_v<T, Types::%s>) Types::%s::to_json(arg, writer);" (trans env orig_name) (trans env orig_name))
            ) cases
         );
        ];
        Line ("}, x);");
      ];
      Line ("}");
    ];
    Block [Line (sprintf "std::string to_json_string(const typedefs::%s &x) {" (cpp_struct_name));
    Block [
      Line ("rapidjson::StringBuffer buffer;");
      Line ("rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);");
      Line ("to_json(x, writer);");
      Line "return buffer.GetString();"
    ];
    Line "}";];
    Line ("}");
  ]
  | _ -> []
  
let sum env loc name cases codegen_type =
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
    List.map (fun x -> Inline (case_class env name x codegen_type)) cases
  in
  let container_class = sum_container env loc name cases codegen_type in
  match codegen_type with
  | Declaration -> 
    [
      Line (sprintf "namespace %s {" (struct_name env name));
      Block [
        Line (sprintf "namespace Types {");
        Block case_classes;
        Line (sprintf "}");
        Line ("");
        Inline container_class;
      ];
      Line (sprintf "}");
    ]
  | Definition ->
    [
      Line (sprintf "namespace %s::Types {" (struct_name env name));
      Block (case_classes |> double_spaced);
      Line (sprintf "}");
      Inline container_class;
    ] |> double_spaced
  | Forward_decl ->
    let cases_forward_declarations =
      List.map (fun (loc, orig_name, unique_name, an, opt_e) -> Line (sprintf "struct %s;" orig_name)) cases 
    in
    [
      Line (sprintf "namespace %s::Types {" (struct_name env name));
      Block cases_forward_declarations;
      Line (sprintf "}");
    ]
  | Variant_typedef -> 
    let type_list =
      List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
        (sprintf "%s::Types::%s" (struct_name env name) (trans env orig_name))
      ) cases
      |> String.concat ", "
    in
    [Line(sprintf "typedef %s %s;" (sprintf "std::variant<%s>" type_list) (struct_name env name))]
  | _ -> []


let enum_container env  loc name cases codegen_type =
  let cpp_struct_name = struct_name env name in
  let cases0, cases1 =
    List.partition (fun (loc, orig_name, unique_name, an, opt_e) ->
      opt_e = None
    ) cases
  in
  let cases0_block =
    if cases0 <> [] then
      [
        Line "if (x.IsString()) {";
        Block (read_cases0 env loc name cases0 Cpp_annot.Enum);
        Line "}";
      ]
    else
      []
  in
  let cases1_block =
    if cases1 <> [] then
      error_at loc "enums with parameters are not supported"
    else
      []
  in
  match codegen_type with
  | Declaration -> 
  [
    Line (sprintf "typedefs::%s from_json(const rapidjson::Value &x);" (cpp_struct_name));
    Inline (from_json_string_declaration (sprintf "typedefs::%s" cpp_struct_name) false);
    Line (sprintf "void to_json(const typedefs::%s &x, rapidjson::Writer<rapidjson::StringBuffer> &writer);" (cpp_struct_name));
    Line (sprintf "std::string to_json_string(const typedefs::%s &x);" (cpp_struct_name));
  ]
  | Definition ->
  [
    Line (sprintf "namespace %s {" (cpp_struct_name));
    Block [
      Line (sprintf "typedefs::%s from_json(const rapidjson::Value &x) {" (cpp_struct_name));
      Block [
        Inline cases0_block;
        Inline cases1_block;
        Line (sprintf "throw _atd_bad_json(\"%s\", x);"
                (single_esc (struct_name env name)))
      ];
      Line "}";
    ];
    Block [Inline (from_json_string_definition (sprintf "typedefs::%s" cpp_struct_name) (None))];
    Block [
      Line (sprintf "void to_json(const typedefs::%s &x, rapidjson::Writer<rapidjson::StringBuffer> &writer) {" (cpp_struct_name));
      Block [
        Line "switch (x) {";
          Block (
            List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
              Line (sprintf "case Types::%s: _atd_write_string(\"%s\", writer); break;" (trans env orig_name) (trans env orig_name))
            ) cases
         );
        Line ("}");
      ];
      Line ("}");
    ];
    Block [Line (sprintf "std::string to_json_string(const typedefs::%s &x) {" (cpp_struct_name));
    Block [
      Line ("rapidjson::StringBuffer buffer;");
      Line ("rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);");
      Line ("to_json(x, writer);");
      Line "return buffer.GetString();"
    ];
    Line "}";];
    Line ("}");
  ]
  | _ -> []

let enum env loc name cases codegen_type =
    let cases =
      List.map (fun (x : variant) ->
        match x with
        | Variant (loc, (orig_name, an), opt_e) ->
            let unique_name = create_struct_name env orig_name in
            (loc, orig_name, unique_name, an, opt_e)
        | Inherit _ -> assert false
      ) cases
    in
    (* let case_classes = 
      List.map (fun x -> Inline (case_class env name x codegen_type)) cases
    in *)
    let container_class = enum_container env loc name cases codegen_type in
    match codegen_type with
    | Declaration -> 
      [
        Line (sprintf "namespace %s {" (struct_name env name));
        Block [
          Line (sprintf "typedef typedefs::%s Types;" (struct_name env name));
          Line ("");
          Inline container_class;
        ];
        Line (sprintf "}");
      ]
    | Definition ->
      [
        Inline container_class;
      ] |> double_spaced
    | Struct_typedef ->
      let type_list =
        List.map (fun (loc, orig_name, unique_name, an, opt_e) ->
          Line (sprintf "%s," (trans env orig_name))) cases in 
          [
            Line (sprintf "enum class %s {" (struct_name env name));
            Block type_list;
            Line (sprintf "};");
          ]
    | _ -> []


let type_def env ((loc, (name, param, an), e) : A.type_def) codegen_type : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  let unwrap e =
    match e with
    | Sum (loc, cases, an) ->
      (match (Cpp_annot.get_cpp_sumtype_repr an) with
      | Variant -> sum env loc name cases codegen_type
      | Enum -> enum env loc name cases codegen_type)
    | Record (loc, fields, an) ->
        record env loc name fields an codegen_type
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Wrap _
    | Name _ -> alias_wrapper env name e codegen_type
    | Shared _ -> not_implemented loc "cyclic references"
    | Tvar _ -> not_implemented loc "parametrized type"
  in
  unwrap e

let module_body env x codegen_type =
  let gen = 
  List.fold_left (fun acc (Type x) -> Inline (type_def env x codegen_type) :: acc) [] x
  |> List.rev
  in
  match codegen_type with
  | Declaration | Definition -> gen |> spaced
  | _ -> gen

let definition_group ~atd_filename env codegen_type
    (is_recursive, (items: A.module_body)) : B.t =
  [
    Inline (module_body env items codegen_type);
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

let to_header_file ~atd_filename ~head (items : A.module_body) dst_path namespace_name =
  let env = init_env () in
  reserve_good_struct_names env items;
  let head = List.map (fun s -> Line s) head in
  let get_lines_for codegen_type =
    Atd.Util.tsort items
    |> List.map (fun x -> Inline (definition_group ~atd_filename env codegen_type x))
  in
  let cpp_forward_declarations = get_lines_for Forward_decl in
  let cpp_forward_declarations = [Line "// forward declarations"] @ cpp_forward_declarations @ [Line ""; Line ""] in
  let cpp_structs_typedefs = get_lines_for Struct_typedef in
  let cpp_variant_typedefs = get_lines_for Variant_typedef in
  let cpp_alias_typedefs = get_lines_for Alias_typedef in
  let cpp_typedefs = 
    [Line "namespace typedefs {"] 
    @ 
    [(Block ([
      Inline cpp_structs_typedefs;
      Inline cpp_variant_typedefs;
      Inline cpp_alias_typedefs;
      ] |> spaced))
    ] @ [Line "} // namespace typedefs"; Line ""] in
  let cpp_declarations = get_lines_for Declaration in
  let namespace_start = [Line (sprintf "namespace %s {" namespace_name)] in
  let namespace_end = [Line (sprintf "} // namespace %s" namespace_name)] in
    Line (fixed_size_preamble_header atd_filename) 
    :: Inline head 
    :: (namespace_start |> double_spaced) 
    @ cpp_forward_declarations 
    @ cpp_typedefs
    @ (cpp_declarations |> double_spaced) 
    @ namespace_end
  |> Indent.to_file ~indent:4 dst_path

let to_cpp_file ~atd_filename (items : A.module_body) dst_path namespace_name =
  let env = init_env () in
  reserve_good_struct_names env items;
  let cpp_defs =
    Atd.Util.tsort items
    |> List.map (fun x -> Inline (definition_group ~atd_filename env Definition x))
  in
  let namespace_end = [Line (sprintf "} // namespace %s" namespace_name)] in
  Line (fixed_size_preamble atd_filename namespace_name) :: cpp_defs @ namespace_end
  |> double_spaced
  |> Indent.to_file ~indent:4 dst_path

let run_file ~tags src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ "_atd"
    |> String.lowercase_ascii
  in
  let hpp_dst_path = dst_name ^ ".hpp" in
  let cpp_dst_path = dst_name ^ ".cpp" in
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
     Cpp_annot.get_cpp_include (snd atd_head) 
    |> List.map (sprintf "#include %s")
  in
  let namespace_name = match Cpp_annot.get_cpp_namespace (snd atd_head) with | Some v -> sprintf "atd::%s" v | None -> "atd" in
  to_header_file ~atd_filename:src_name ~head atd_module hpp_dst_path namespace_name;
  to_cpp_file ~atd_filename:src_name atd_module cpp_dst_path namespace_name
