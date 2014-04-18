(* Names *)

open Atdj_env

(* Translate type names into idiomatic Java class names.  We special case
 * `string', `int' and `bool'  (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name ?(require_class = false) str =
  let str = Str.global_replace (Str.regexp "\\[\\]") "Array" str in
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
