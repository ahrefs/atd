module A = Atd.Ast
module B = Indent

type env = {
  create_variable: string -> string;
  translate_variable: string -> string;
}

let type_name env name = 
  env.translate_variable name

let init_env () : env =
  (* The list of "keywords" is extracted from
      https://en.cppreference.com/w/cpp/keyword
  *)
  let keywords = [
    (* Reserved Words *)
    "alignas"; "alignof"; "and"; "and_eq"; "asm"; "atomic_cancel"; 
    "atomic_commit"; "atomic_noexcept"; "auto"; "bitand"; "bitor"; 
    "bool"; "break"; "case"; "catch"; "case"; "catch"; "char";
    "char8_t"; "char16_t"; "char32_t"; "class"; "compl"; "concept"; 
    "const"; "consteval"; "constexpr"; "constinit"; "const_cast";
    "continue"; "co_await"; "co_return"; "co_yield"; "decltype"; 
    "default"; "delete"; "do"; "double"; "dynamic_cast"; "else";
    "enum"; "explicit"; "export"; "extern"; "false"; "float"; "for";
    "friend"; "goto"; "if"; "inline"; "int"; "long"; "mutable";
    "namespace"; "new"; "noexcept"; "not"; "not_eq"; "nullptr";
    "operator"; "or"; "or_eq"; "private"; "protected"; "public";
    "reflexpr"; "register"; "reinterpret_cast"; "requires"; "return";
    "short"; "signed"; "signed"; "sizeof"; "static"; "static_assert";
    "static_cast"; "struct"; "switch"; "synchronized"; "template";
    "this"; "thread_local"; "throw"; "true"; "try"; "typedef";
    "typeid"; "typename"; "union"; "unsigned"; "using"; "virtual";
    "void"; "volatile"; "wchar_t"; "while"; "xor"; "xor_eq";
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
    create_variable;
    translate_variable;
  }