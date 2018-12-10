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

(* Translate type names into idiomatic Flow names.  We special case
 * `string', `int', `bool' and `float` (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_flow_name str =
  match str with
    | "string" -> "string"
    | "float"
    | "int" -> "number"
    | "bool"   -> "boolean"
    | _ -> to_camel_case str

(* Per https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords *)
let js_keywords = [
  "await";
  "break";
  "case";
  "catch";
  "class";
  "const";
  "continue";
  "debugger";
  "default";
  "delete";
  "do";
  "else";
  "enum";
  "export";
  "extends";
  "finally";
  "for";
  "function";
  "if";
  "implements";
  "import";
  "in";
  "instanceof";
  "interface";
  "let";
  "new";
  "package";
  "private";
  "protected";
  "public";
  "return";
  "static";
  "super";
  "switch";
  "this";
  "throw";
  "try";
  "typeof";
  "var";
  "void";
  "while";
  "with";
  "yield";

  (* Previous "future" reserved keywords. Do we still need to avoid these? *)
  "abstract";
  "boolean";
  "byte";
  "char";
  "double";
  "final";
  "float";
  "goto";
  "int";
  "long";
  "native";
  "short";
  "synchronized";
  "throws";
  "transient";
  "volatile";
]

let is_js_keyword =
  let tbl = Hashtbl.create 200 in
  List.iter (fun k -> Hashtbl.add tbl k ()) js_keywords;
  fun k -> Hashtbl.mem tbl k

(*
   Automatically append an underscore to a field name if it is a JS keyword.
   Use the alternative provided as <js name ="...">  or <json name ="..."> if available.

   ATD field                                 Flow name

   not_a_keyword                             not_a_keyword
   case                                      case_
   case <json name="cas">                    cas
   case <js name="kase"> <json name="cas">   kase

*)
let get_js_field_name field_name annot =
  let field_name = get_json_field_name field_name annot in
  let field_name =
    if is_js_keyword field_name then
      field_name ^ "_"
    else
      field_name
  in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:field_name
    ~sections:["js"]
    ~field:"name"
    annot

let get_flow_type_name field_name annot =
  let lower_field_name = String.lowercase_ascii field_name in
  let field_name =
    if is_js_keyword lower_field_name then
      field_name ^ "_"
    else
      field_name
  in
  let field_name =
    Atd.Annot.get_field
      ~parse:(fun s -> Some s)
      ~default:field_name
      ~sections:["js"]
      ~field:"name"
      annot
  in
  to_camel_case field_name
