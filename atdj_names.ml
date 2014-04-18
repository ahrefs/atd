(* Names *)

open Atdj_env

(* Translate type names into idiomatic Java class names.  We special case
 * `string', `int' and `bool'  (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name ?(require_class = false) str =
  match str with
    | "string" -> "String"
    | "int"    -> if require_class then "Integer" else "int"
    | "bool"   -> if require_class then "Boolean" else "boolean"
    | "float"  -> if require_class then "Double"  else "double"
    | _ ->
        let res    = String.copy str in
        let offset = ref 0 in
        let upper  = ref true in
        let f = function
          | '_' ->
              upper := true;
          | ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') as x ->
              upper := true;
              res.[!offset] <- x;
              incr offset
          | _ as x ->
              if !upper then (
                res.[!offset] <- Char.uppercase x;
                upper := false
              ) else
                res.[!offset] <- x;
              incr offset in
        String.iter f str;
        String.sub res 0 !offset

(* Generate a unique name by appending, if necessary, an integer
 * suffix to the string.  For example, after successive calls with the name
 * `foo', we obtain `foo', `foo1', `foo2' etc. *)
let freshen env str =
  if Names.mem str env.names then
    let n = succ (Names.find str env.names) in
    let env = { env with names = Names.add str n env.names } in
    (env, str ^ (string_of_int n))
  else
    let env = { env with names = Names.add str 0 env.names } in
    (env, str)

let java_keywords = [
  "abstract";
  "assert";
  "boolean";
  "break";
  "byte";
  "case";
  "catch";
  "char";
  "class";
  "const";
  "continue";
  "default";
  "do";
  "double";
  "else";
  "enum";
  "extends";
  "final";
  "finally";
  "float";
  "for";
  "goto";
  "if";
  "implements";
  "import";
  "instanceof";
  "int";
  "interface";
  "long";
  "native";
  "new";
  "package";
  "private";
  "protected";
  "public";
  "return";
  "short";
  "static";
  "strictfp";
  "super";
  "switch";
  "synchronized";
  "this";
  "throw";
  "throws";
  "transient";
  "try";
  "void";
  "volatile";
  "while";
]

let is_java_keyword =
  let tbl = Hashtbl.create 200 in
  List.iter (fun k -> Hashtbl.add tbl k ()) java_keywords;
  fun k -> Hashtbl.mem tbl k

(*
   Automatically append an underscore to a field name if it is a Java keyword.
   Use the alternative provided as <java name ="..."> if available.

   ATD field                           Java name

   not_a_keyword                       not_a_keyword
   class                               class_
   class <java name="class_name">      class_name
   not_a_keyword <java name="class">   class

*)
let name_field field_name annot =
  let field_name =
    if is_java_keyword field_name then
      field_name ^ "_"
    else
      field_name
  in
  Atd_annot.get_field (fun s -> Some s) field_name ["java"] "name" annot
