open Atd.Import
(* Names *)

let to_camel_case (s : string) =
  let res    = Bytes.of_string s in
  let offset = ref 0 in
  let upper  = ref true in
  let f = function
    | '_' ->
        upper := true;
    | ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') as x ->
        upper := true;
        Bytes.set res !offset x;
        incr offset
    | _ as x ->
        if !upper then (
          Bytes.set res !offset (Char.uppercase_ascii x);
          upper := false
        ) else
          Bytes.set res !offset x;
        incr offset in
  String.iter f s;
  Bytes.to_string (Bytes.sub res 0 !offset)

(* Translate type names into idiomatic Scala names.  We special case
 * `string', `int', `bool' and `float` (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name str =
  match str with
    | "string" -> "std::string"
    | "int"    -> "int32_t"
    | "bool"   -> "boolean"
    | "float"  -> "float"
    | _ -> to_camel_case str

(* Per https://cpp-lang.org/files/archive/spec/2.12/01-lexical-syntax.html *)
let cpp_keywords = [
  "alignas";
  "alignof";
  "and";
  "and_eq";
  "asm";
  "atomic_cancel";
  "atomic_commit";
  "atomic_noexcept";
  "auto";
  "bitand";
  "bitor";
  "bool";
  "break";
  "case";
  "catch";
  "char";
  "char8_t";
  "char16_t";
  "char32_t";
  "class";
  "compl";
  "concept";
  "const";
  "consteval";
  "constexpr";
  "constinit";
  "const_cast";
  "continue";
  "co_await";
  "co_return";
  "co_yield";
  "decltype";
  "default";
  "delete";
  "do";
  "double";
  "dynamic_cast";
  "else";
  "enum";
  "explicit";
  "extern";
  "false";
  "float";
  "for";
  "friend";
  "goto";
  "if";
  "inline";
  "int";
  "long";
  "mutable";
  "namespace";
  "new";
  "noexcept";
  "not";
  "not_eq";
  "nullptr";
  "operator";
  "or";
  "or_eq";
  "private";
  "protected";
  "public";
  "reflexpr";
  "register";
  "reinterpret_cast";
  "requires";
  "return";
  "short";
  "signed";
  "sizeof";
  "static";
  "static_assert";
  "static_cast";
  "struct";
  "switch";
  "synchronized";
  "template";
  "this";
  "thread_local";
  "throw";
  "true";
  "true";
  "try";
  "typedef";
  "typeid";
  "typename";
  "union";
  "unsigned";
  "using";
  "virtual";
  "void";
  "volatile";
  "wchar_t";
  "while";
  "xor";
  "xor_eq";
]

let is_cpp_keyword =
  let tbl = Hashtbl.create 200 in
  List.iter (fun k -> Hashtbl.add tbl k ()) cpp_keywords;
  fun k -> Hashtbl.mem tbl k

(*
   Automatically append an underscore to a field name if it is a CPP keyword.
   Use the alternative provided as <cpp name ="..."> if available.

   ATD field                           CPP name

   not_a_keyword                       not_a_keyword
   class                               class_
   class <cpp name="class_name">     class_name
   not_a_keyword <cpp name="class">  class

*)
let get_cpp_field_name field_name annot =
  let field_name =
    if is_cpp_keyword field_name then
      field_name ^ "_"
    else
      field_name
  in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["cpp"]
    ~field:"name"
    annot

let get_cpp_variant_name field_name annot =
  let lower_field_name = String.lowercase_ascii field_name in
  let field_name =
    if is_cpp_keyword lower_field_name then
      field_name ^ "_"
    else
      field_name
  in
  let field_name =
    Atd.Annot.get_field
      ~parse:(fun s -> Some s)
      ~default:field_name
      ~sections:["cpp"]
      ~field:"name"
      annot
  in
  to_camel_case field_name


let get_json_field_name field_name annot =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["json"]
    ~field:"name"
    annot

let get_json_variant_name field_name annot =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["json"]
    ~field:"name"
    annot

(* Splits a package name into a prefix and last component.
   Eg: "com.example.foo.bar" -> ("com.example.foo", "bar")
*)
let split_package_name p =
  let dot = String.rindex p '.' in
  let prefix = String.sub p 0 dot in
  let suffix = String.sub p (dot + 1) (String.length p - dot - 1) in
  (prefix, suffix)
