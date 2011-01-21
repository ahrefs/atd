(* Copyright (c) 2011, MyLife.com, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *    - Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    - Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    - Neither the name of the <organization> nor the
 *      names of its contributors may be used to endorse or promote products
 *      derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(* TODO
 * - Clean up constructors for option types
 * - Change naming for variant constructors
 * - Add support for lists of lists
 * - Add location information to exceptions (need own parser)
 *)

open Printf
open Atd_ast


(* --------------------------------------------------------------------------- *)
(* Translation environment *)

type id = string
type ty_name = string

(* Java types *)
type ty =
  [ `Class of ty_name * (id * ty_name) list  (* Class name and constructor parameters *)
  | `Interface of ty_name                    (* Interface name *)
  ]

module Names = Map.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
)

type env_t =
  { module_items : (string * type_expr) list
  ; package      : string
  ; package_dir  : string
  ; input_file   : string option
  ; graph        : bool
  ; types        : ty list
  ; sub_types    : (ty_name * ty_name) list
  ; names        : int Names.t
  }

let default_env =
  { module_items = []
  ; package      = "out"
  ; package_dir  = "out"
  ; input_file   = None
  ; graph        = false              (* Whether to ouput a dot graph *)
  ; types        = []                 (* Generated Java types *)
  ; sub_types    = []                 (* Sub-typing relation amongst types *)
  ; names        = Names.empty        (* Current used names, for freshening *)
  }


(* --------------------------------------------------------------------------- *)
(* Names *)

(* Translate type names into idiomatic Java class names.  We special case
 * `string', `int' and `bool'  (see code).  For the remainder, we remove
 * underscores and capitalise any character that is immediately following
 * an underscore or digit.  We also capitalise the initial character
 * e.g. "foo_bar42baz" becomes "FooBar42Baz". *)
let to_class_name str =
  let str = Str.global_replace (Str.regexp "\\[\\]") "Array" str in
  match str with
    | "string" -> "String"
    | "int"    -> "int"
    | "bool"   -> "boolean"
    | "float"  -> "double"
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


(* --------------------------------------------------------------------------- *)
(* Utilies *)

(* Normalise an ATD type by expanding `top-level' type aliases *)
let rec norm_ty env atd_ty =
  match atd_ty with
    | `Name (_, (_, name, _), _) ->
        (match name with
           | "bool" | "int" | "float" | "string" -> atd_ty
           | _ -> norm_ty env (List.assoc name env.module_items)
        )
    | _ -> atd_ty

(* Calculate the JSON representation of an ATD type.
 *
 * Values of sum types t are encoded as either Strings or two-element
 * JSONArrays, depending upon the arity of the particular constructor.
 * A nullary constructor C is denoted by the String "C", whilst
 * an application of a unary constructor C to an ATD value v is denoted by the
 * JSONArray ["C", <v>], where <v> is the JSON representation of v.
 *
 * Option types follow the same encoding as sum types; the value None
 * is represented by the String "None", whilst a value Some v is denoted by the
 * JSONArray ["Some", <v>].
 *)
let json_of_atd env atd_ty =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `Sum    _              (* Either a String or a two element JSONArray *)
    | `Option _ -> "Object"  (* Either a String or a two element JSONArray *)
    | `Record _ -> "JSONObject"
    | `List   _ -> "JSONArray"
    | `Name (_, (_, ty, _), _) ->
        (match ty with
           | "bool"   -> "boolean"
           | "int"    -> "int"
           | "float"  -> "double"
           | "string" -> "String"
           | _        -> assert false
        )
    | _ -> failwith "Not yet implemented"

(* Calculate the method name required to extract the JSON representation of an
 * ATD value from either a JSONObject or a JSONArray.
 *)
let get env atd_ty opt =
  let atd_ty = norm_ty env atd_ty in
  let prefix = if opt then "opt" else "get" in
  let suffix =
    match atd_ty with
      | `Sum _ | `Option _ -> ""
      | _ -> String.capitalize (json_of_atd env atd_ty) in
  prefix ^ suffix

(* Assignment with translation.  Suppose that atd_ty is an ATD type, with
 * corresponding Java and (Javafied) JSON types java_ty and json_ty. Then this
 * function assigns to a variable `dst' of type java_ty from a variable `src' of
 * type `json_ty'.
 *)
let rec assign env dst src java_ty atd_ty indent =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `Sum _ ->
        sprintf "%s%s = %sFactory.make(%s);\n" indent dst java_ty src
    | `Record _ | `Option _ ->
        sprintf "%s%s = new %s(%s);\n" indent dst java_ty src
    | `List (_, ty, _) ->  (* FIXME This is a bodge *)
          sprintf "%s%s = new %s;\n" indent dst
            (Str.global_replace (Str.regexp "\\[\\]") ("[" ^ src ^ ".length()]") java_ty)
        ^ sprintf "%sfor (int i = 0; i < %s.length(); ++i) {\n" indent src
        ^ sprintf "%s  %s tmp = %s.%s(i);\n" indent
          (json_of_atd env ty) src (get env ty false)
        ^ assign env (dst ^ "[i]") "tmp"
          (Str.global_replace (Str.regexp "\\[\\]") "" java_ty) ty (indent ^  "  ")
        ^ sprintf "%s}\n" indent
    | `Name (_, (_, ty, _), _) ->
        (match ty with
           | "bool" | "int" | "float" | "string" ->
               sprintf "%s%s = %s;\n" indent dst src
           | _  -> assert false
        )
    | _ -> failwith "Not yet implemented"

(* Assign from an object field, with support for optional fields.  The are two
 * kinds of optional fields: `With_default (~) and `Optional (?).  For both
 * kinds, we return the following values if the field is absent:
 *
 *   bool   -> false
 *   int    -> 0
 *   float  -> 0.0
 *   string -> ""
 *   list   -> []
 *   option -> None
 *
 * Optional fields of record and sum types abort compilation (we do not yet
 * support default values for these types).
 *
 * Fields of the `Optional kind extend this behaviour by automatically lifting
 * values of type t to option t by wrapping within a `Some'.  Hence `Optional may
 * only be applied to fields of type option t. Note that absent fields are still
 * assigned `None', as before.
 *
 * For `With_default fields, of types bool, int, float, string and list, we use
 * the org.json opt methods to extract the field.  These methods already return
 * the appropriate defaults if field is absent.  For option types, we manually
 * check for the field and manually create a default.  If the field is present,
 * then we wrap its values as necessary.
 *)
let assign_field env (`Field (_, (name, kind, annots), atd_ty)) java_ty =
  (* Check whether the field is optional *)
  let is_opt =
    match kind with
      | `Optional | `With_default -> true
      | `Required -> false in
  let f () =
    let src = sprintf "jo.%s(\"%s\")" (get env atd_ty is_opt) name in
    assign env name src java_ty atd_ty "    " in
  if is_opt then
    match norm_ty env atd_ty with
      | `Name _ -> f ()  (* Primitive types, such as bool, int etc. *)
      | `List _ -> 
          let src = sprintf "jo.%s(\"%s\")" (get env atd_ty is_opt) name in
            sprintf "    if (jo.has(\"%s\")) {\n" name
          ^ assign env name src java_ty atd_ty "      " 
          ^ sprintf "    } else {\n"
          ^ sprintf "      %s = new %s;\n" name
              (Str.global_replace (Str.regexp "\\[\\]") "[0]" java_ty)
          ^ sprintf "    }\n"
      | `Option (_, sub_atd_ty, _) ->
          (match kind with
             | `Optional ->
                 (* Lift *)
                 sprintf "    if (jo.has(\"%s\")) {\n" name
                 ^ sprintf "      Object[] a = new Object[2];\n"
                 ^ sprintf "      a[0] = new String(\"Some\");\n"
                 ^ sprintf "      a[1] = jo.%s(\"%s\");\n" (get env sub_atd_ty false) name
                 ^ sprintf "      %s = new %s(new JSONArray(a));\n" name java_ty
                 ^ sprintf "    } else {\n"
                 ^ sprintf "      %s = new %s();\n" name java_ty
                 ^ sprintf "    }\n"
             | `With_default ->
                 (* Already lifted *)
                   sprintf "    if (jo.has(\"%s\"))\n" name
                 ^ sprintf "      %s = new %s(jo);\n" name java_ty
                 ^ sprintf "    else\n"
                 ^ sprintf "      %s = new %s();\n" name java_ty
             | _ -> assert false
          )
      | _ -> failwith "Not yet supported"
  else
    f ()

(* Check whether a type supports an accept method *)
let rec can_accept env atd_ty =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `Sum    _ -> true
    | `Record _ -> true
    | `Name   _ -> false
    | `Option (_, sub_atd_ty, _) ->
        can_accept env sub_atd_ty
    | `List (_, sub_atd_ty, _) ->
        can_accept env sub_atd_ty
    | _ -> failwith "Not yet implemented"

(* Generate an accept method for a type *)
let accept env var java_ty atd_ty visitor indent =
  if not (can_accept env atd_ty) then ""
  else
    let atd_ty = norm_ty env atd_ty in
    match atd_ty with
      | `List _ ->
          let java_sub_ty = (Str.global_replace (Str.regexp "\\[\\]") "" java_ty) in
            sprintf "%sfor (%s elt : %s)\n" indent java_sub_ty var
          ^ sprintf "%s  elt.accept(%s);\n" indent visitor
      | _ ->
          sprintf "%s%s.accept(%s);\n" indent var visitor

(* Generate a toString command *)
let rec to_string env id atd_ty lift_opt indent =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `List (_, atd_sub_ty, _) ->
          sprintf "%sstr += Util.indent(indent) + \"[\\n\";\n" indent
        ^ sprintf "%sindent += 2;\n" indent
        ^ sprintf "%sfor (int i = 0; i < %s.length; ++i) {\n" indent id
        ^ (match norm_ty env atd_sub_ty with
             | `Name _ -> sprintf "%s  str += Util.indent(indent);\n" indent
             | _ -> ""
          )
        ^ to_string env (id ^ "[i]") atd_sub_ty false (indent ^ "  ")
        ^ sprintf "%s  if (i < %s.length - 1)\n" indent id
        ^ sprintf "%s    str += \",\\n\";\n" indent
        ^ sprintf "%s  else\n" indent
        ^ sprintf "%s    str += \"\\n\";\n" indent
        ^ sprintf "%s}\n" indent
        ^ sprintf "%sindent -= 2;\n" indent
        ^ sprintf "%sstr += Util.indent(indent) + \"]\";\n" indent
    | `Option _ ->
        sprintf "%sstr += %s.toString(indent, %b);\n" indent id lift_opt
    | `Name (_, (_, "string", _), _) ->  (* TODO Check that this is the correct behaviour *)
        sprintf "%sstr += \"\\\"\" + Util.escape(String.valueOf(%s)) + \"\\\"\";\n" indent id
    | `Name _ ->
        sprintf "%sstr += String.valueOf(%s);\n" indent id
    | _ ->
        sprintf "%sstr += %s.toString(indent);\n" indent id

(* Generate a toString command for a record field.  For brevity, we omit
 * optional fields that have their default value. *)
let to_string_field env = function
  | (`Field (_, (name, kind, annots), atd_ty)) ->
      let atd_ty = norm_ty env atd_ty in
      (* In the case of an optional field, create a predicate to test whether
       * the field has its default value. *)
      let pred =
        let is_opt =
          match kind with
            | `Optional | `With_default -> true
            | `Required -> false in
        if is_opt then
          Some
            (match atd_ty with
               | `Option _ ->
                   sprintf "%s.is_set" name
               | `List _ ->
                   sprintf "%s.length > 0" name
               | `Name (_, (_, sub_name, _), _) ->
                   (match sub_name with
                      | "bool" ->
                          sprintf "%s" name
                      | "int" | "float" ->
                          sprintf "%s != 0" name
                      | "string" ->
                          sprintf "%s.equals(\"\") == false" name
                      | _ -> assert false
                   )
               | _ -> assert false
            )
        else None in
      let lift_opt =
          match kind with
            | `Optional -> true | _ -> false in
      let (prefix, suffix, indent) =
        match pred with
          | Some p ->  (sprintf "    if (%s) {\n" p, "    }\n", "      ")
          | None   ->  ("", "", "    ") in
        prefix
      ^ sprintf "%sstr += Util.indent(indent + 2) + \"\\\"%s\\\": \";\n" indent name
      ^ sprintf "%sindent += 4;\n" indent
      ^ (match atd_ty with
           | `Name _ -> ""
           | `Option (_, atd_sub_ty, _) ->
               let atd_sub_ty = norm_ty env atd_sub_ty in
               (match atd_sub_ty with
                  | `Name _ | `Sum _ -> ""
                  | _ ->  (* Output a newline *)
                      sprintf "%sstr += \"\\n\";\n" indent
               )
           | _ -> sprintf "%sstr += \"\\n\";\n" indent
        )
      ^ to_string env name atd_ty lift_opt indent
      ^ sprintf "%sindent -= 4;\n" indent
      ^ sprintf "%sstr += \",\\n\";\n" indent
      ^ suffix

(* Generate a hashCode command *)
let hash_code env atd_ty java_ty id res indent =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `Name _ ->
        let class_name =
          match java_ty with
            | "int" -> "Integer"
            | _     -> String.capitalize java_ty in
        sprintf "%s%s += new %s(%s).hashCode();\n" indent res class_name id
    | `List (_, sub_atd_ty, _) ->
        let sub_atd_ty = norm_ty env sub_atd_ty in
        (match sub_atd_ty with
           | `Name _ ->
               let java_sub_ty = Str.global_replace (Str.regexp "\\[\\]$") "" java_ty in
               let class_name =
                 match java_sub_ty with
                   | "int" -> "Integer"
                   | _     -> String.capitalize java_sub_ty in
                 sprintf "%sfor (int i = 0; i < %s.length; ++i)\n" indent id
               ^ sprintf "%s  %s *= 31 + new %s(%s[i]).hashCode();\n" indent res class_name id
           | _ ->
                 sprintf "%sfor (int i = 0; i < %s.length; ++i)\n" indent id
               ^ sprintf "%s  %s *= 31 + %s[i].hashCode();\n" indent res id
        )
    | _       -> sprintf "%s%s += %s.hashCode();\n" indent res id

(* Generate a compareTo command *)
let compare_to env atd_ty java_ty this that res indent =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
    | `Name _ ->
        let class_name =
          match java_ty with
            | "int" -> "Integer"
            | _     -> String.capitalize java_ty in
        sprintf "%s%s = new %s(%s).compareTo(%s);\n" indent res class_name this that
    | `List (_, sub_atd_ty, _) ->
        let sub_atd_ty = norm_ty env sub_atd_ty in
        (match sub_atd_ty with
           | `Name _ ->
               sprintf "%s%s = Util.compareTo(%s, %s);\n" indent res this that
           | _ ->
                 sprintf "%s{\n" indent
               ^ sprintf "%s  int i = 0;\n" indent
               ^ sprintf "%s  int minLen = Math.min(%s.length, %s.length);\n" indent this that
               ^ sprintf "%s  for (; i < minLen; ++i) {\n" indent
               ^ sprintf "%s    int mycmp = %s[i].compareTo(%s[i]);\n" indent this that
               ^ sprintf "%s    if (mycmp != 0) {\n" indent
               ^ sprintf "%s      // Found differing elements; we are done\n" indent
               ^ sprintf "%s      %s = mycmp;\n" indent res
               ^ sprintf "%s      break;\n" indent
               ^ sprintf "%s    }\n" indent
               ^ sprintf "%s  }\n" indent
               ^ sprintf "%s  if (i == minLen) {\n" indent
               ^ sprintf "%s    // Identical prefixes; find shorter list\n" indent
               ^ sprintf "%s    if (%s.length == minLen)\n" indent this
               ^ sprintf "%s      %s = -1;\n" indent res
               ^ sprintf "%s    else if (%s.length == minLen)\n" indent that
               ^ sprintf "%s      %s = 1;\n" indent res
               ^ sprintf "%s    else\n" indent
               ^ sprintf "%s      %s = 0;\n" indent res
               ^ sprintf "%s  }\n" indent
               ^ sprintf "%s}\n" indent
        )
    | _  -> sprintf "%s%s = %s.compareTo(%s);\n" indent res this that

(* Generate a javadoc comment *)
let javadoc loc annots indent =
  let from_inline_text text = indent ^ " * " ^ text ^ "\n" in
  (* Assume that code is the name of a field that is defined in the same class *)
  let from_inline_code code = indent ^ " * {@link #" ^ code ^ "}\n" in
  let from_doc_para acc para =
    List.fold_left
      (fun acc -> function
         | `Text text -> (from_inline_text text) :: acc
         | `Code code -> (from_inline_code code) :: acc
      )
      acc
      para in
  let from_doc = function
    | `Text blocks ->
        List.fold_left
          (fun acc -> function
             | `Paragraph para -> from_doc_para acc para
             | `Pre _          -> failwith "Not yet implemented"
          )
          []
          blocks in
  (match Ag_doc.get_doc loc annots with
     | Some doc ->
         let header = indent ^ "/**\n" in
         let footer = indent ^ " */\n" in
         let body   =
           String.concat "" (List.rev (from_doc doc)) in
         header ^ body ^ footer
     | None     -> ""
  )


(* --------------------------------------------------------------------------- *)
(* Translation of ATD types into Java types *)

(* For option, sum and record types, we generate a Java class.  Each such class
 * implements the following interface:
 *
 *  interface Atdj {
 *    String toString();
 *    String toString(int indent);
 *    Visitor accept(Visitor v);
 *  }
 *
 * The toString(int indent) method outputs a JSON representation of the
 * associated value.  The indent level is required for nicely formatting
 * nested types.   The toString() method simply invokes the previous method
 * with an indent level of zero.  The accept(Visitor v) method accepts visitors
 * to the class (see below).
 *
 * Each class also has a String constructor for a JSON string as well as a
 * constructor from the corresponding org.json type (see json_of_atd, above).
 *
 * We do not generate classes for types bool, int, float, string and list;
 * instead we `inline' these types directly into the class in which they
 * occur.  We do this so that the Java programmer can access such values
 * directly, thereby avoiding the overhead of having to manually unbox each such
 * value upon access.
 *)

let open_class env cname =
  let out = open_out (env.package_dir ^ "/" ^ cname ^ ".java") in
  fprintf out "// Automatically generated; do not edit\n\n";
  fprintf out "package %s;\n\n" env.package;
  fprintf out "import org.json.*;\n";
  fprintf out "import java.util.Arrays;\n\n";
  fprintf out "import java.lang.Math;\n\n";
  out

let rec trans_module env items = List.fold_left trans_outer env items

and trans_outer env (`Type (_, (name, _, _), atd_ty)) =
  match atd_ty with
    | `Sum _ as s ->
        trans_sum name env s
    | `Record _ as r ->
        trans_record name env r
    | `Name (_, (_, name, _), _) ->
        (* Don't translate primitive types at the top-level *)
        env
    | _ -> failwith "Not yet implemented"

(* Translation of sum types.  For a sum type
 *
 *   type t = Foo | Bar,
 *
 * we generate:
 *
 * 1. An marker interface
 * 2. A class for each constructor (i.e. Foo and Bar), implementing the marker
 *    interface.
 * 3. A factory class with a static `make' method.  This method accepts a JSON parameter
 *    corresponding to a t, and instantiates the appropriate class from (2) .
 *)
and trans_sum my_name env (`Sum (loc, vars, annots)) =
  (* Interface *)
  let ifc_name = to_class_name my_name in
  let ifc_out = open_class env ifc_name in
  output_string ifc_out (javadoc loc annots "");
  fprintf ifc_out "public interface %s extends Atdj {\n" ifc_name;
  fprintf ifc_out "  /**\n";
  fprintf ifc_out "   * Get the the constructor index.  For example, given an ATD type\n";
  fprintf ifc_out "   * <pre>\n";
  fprintf ifc_out "   *   type t =\n";
  fprintf ifc_out "   *     [ Foo of ...\n";
  fprintf ifc_out "   *     | Bar of ...\n";
  fprintf ifc_out "   *     ],\n";
  fprintf ifc_out "   * </pre>\n";
  fprintf ifc_out "   * {@code Foo} values have index {@code 0} and {@code Bar} values have index {@code 1}.\n";
  fprintf ifc_out "   * @return The constructor index.\n";
  fprintf ifc_out "   */\n";
  fprintf ifc_out "  int getIndex();\n";
  fprintf ifc_out "  /**\n";
  fprintf ifc_out "   * Comparison, using a total order.\n";
  fprintf ifc_out "   * @return The result of the comparison.\n";
  fprintf ifc_out "   */\n";
  fprintf ifc_out "  int compareTo(%s that);\n" ifc_name;
  fprintf ifc_out "  boolean equals(%s that);\n" ifc_name;
  fprintf ifc_out "}\n";
  close_out ifc_out;
  let env = { env with types = (`Interface ifc_name) :: env.types; } in
  (* Constructors *)
  (* Javadoc doesn't seem to be used here, so omit for now *)
  let (env, names, _) = List.fold_left
    (fun (env, names, count) -> function
       | `Variant (_, (var_name, _), atd_type_expr_opt) ->
           let (env, var_class_name) = freshen env (to_class_name var_name) in
           let out = open_class env var_class_name in
           fprintf out "public class %s implements %s {\n"  var_class_name ifc_name;
           let env =
             (match atd_type_expr_opt with
                | None ->
                    fprintf out "  %s() { }\n" var_class_name;
                    fprintf out "\n";
                    fprintf out "  public Visitor accept(Visitor v) {\n";
                    fprintf out "    v.visit(this);\n";
                    fprintf out "    return v;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public String toString() {\n";
                    fprintf out "    return toString(0);\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public String toString(int indent) {\n";
                    fprintf out "    return \"\\\"%s\\\"\";\n" var_name;
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public int hashCode() {\n";
                    fprintf out "    return 31 * %d;\n" count;
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public int compareTo(%s that) {\n" ifc_name;
                    fprintf out "    return this.index - that.getIndex();\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  /**\n";
                    fprintf out "   * Test for equality.\n";
                    fprintf out "   * @return Whether the two objects are equal.\n";
                    fprintf out "   */\n";
                    fprintf out "  public boolean equals(%s that) {\n" ifc_name;
                    fprintf out "    return this.compareTo(that) == 0;\n";
                    fprintf out "  }\n";
                    fprintf out "  public int getIndex() {\n";
                    fprintf out "    return index;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    { env with
                        types = `Class (var_class_name, []) :: env.types;
                        sub_types = (var_class_name, ifc_name) :: env.sub_types
                    }
                | Some atd_ty ->
                    let (java_ty, env) = trans_inner env atd_ty in
                    fprintf out "  %s(%s value) throws JSONException {\n"
                      var_class_name (json_of_atd env atd_ty);
                    fprintf out "%s\n" (assign env "this.value" "value" java_ty atd_ty "    ");
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public Visitor accept(Visitor v) {\n";
                    fprintf out "    v.visit(this);\n";
                    output_string out (accept env "value" java_ty atd_ty "v" "    ");
                    fprintf out "    return v;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public String toString() {\n";
                    fprintf out "    return toString(0);\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public String toString(int indent) {\n";
                    fprintf out "    String str = \"\";\n";
                    fprintf out "    str += Util.indent(indent)";  (* Line continued below *)
                    fprintf out "+ \"[\\n\" + Util.indent(indent + 2) + \"\\\"%s\\\",\\n\";" var_name;
                    fprintf out "    indent += 2;\n";
                    fprintf out "    %s" (to_string env "value" atd_ty false "");
                    fprintf out "    indent -= 2;\n";
                    fprintf out "    str += \"\\n\" + Util.indent(indent) + \"]\";\n";
                    fprintf out "    return str;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public int hashCode() {\n";
                    fprintf out "    int h = 31 * %d;\n" count;
                    output_string out (hash_code env atd_ty java_ty "value" "h" "    ");
                    fprintf out "    return h;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public int compareTo(%s that) {\n" ifc_name;
                    fprintf out "    int cmp = this.index - that.getIndex();\n";
                    fprintf out "    if (cmp == 0) {\n";
                    output_string out
                      (compare_to env
                         atd_ty
                         java_ty
                         "this.value"
                         ("((" ^ var_class_name ^ ")that).value")
                         "cmp"
                         "      "
                      );
                    fprintf out "    }\n";
                    fprintf out "    return cmp;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  /**\n";
                    fprintf out "   * Test for equality.\n";
                    fprintf out "   * @return Whether the two objects are equal.\n";
                    fprintf out "   */\n";
                    fprintf out "  public boolean equals(%s that) {\n" ifc_name;
                    fprintf out "    return this.compareTo(that) == 0;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public int getIndex() {\n";
                    fprintf out "    return index;\n";
                    fprintf out "  }\n";
                    fprintf out "\n";
                    fprintf out "  public final %s value;\n" java_ty;
                    { env with
                        types = `Class (var_class_name, ["value", java_ty]) :: env.types;
                        sub_types = (var_class_name, ifc_name) :: env.sub_types
                    }
             ) in
           fprintf out "  private final int index = %d;\n" count;
           fprintf out "}\n";
           close_out out;
           (env, (var_name, var_class_name) :: names, succ count)
       | `Inherit _ -> assert false
    )
    (env, [], 1) vars in
  (* Factory class *)
  let fact_name = to_class_name (my_name ^ "Factory") in
  let fact_out = open_class env fact_name in
  fprintf fact_out "/**\n";
  fprintf fact_out " * Construct objects of type %s.\n" my_name;
  fprintf fact_out " */\n";
  fprintf fact_out "public class %s {\n" fact_name;
  fprintf fact_out "  public static %s make(String s) throws JSONException {\n" ifc_name;
  fprintf fact_out "    try {\n";
  fprintf fact_out "      return make(new JSONArray(s));\n";
  fprintf fact_out "    } catch (Exception e) {\n";
  fprintf fact_out "      // Could not parse as JSONArray, so try as string\n";
  fprintf fact_out "      return make((Object)Util.unescapeString(Util.parseJSONString(s)));\n";
  fprintf fact_out "    }\n";
  fprintf fact_out "  }\n";
  fprintf fact_out "\n";
  fprintf fact_out "  static %s make(Object o) throws JSONException {\n" ifc_name;
  fprintf fact_out "    String tag = Util.tag(o);\n";
  List.iter
    (function
       | `Variant (_, (var_name, _), type_expr_opt) ->
           (match type_expr_opt with
              | None ->
                  fprintf fact_out "    if (tag.equals(\"%s\"))\n" var_name;
                  fprintf fact_out "      return new %s();\n" (List.assoc var_name names)
              | Some t ->
                  fprintf fact_out "    if (tag.equals(\"%s\"))\n" var_name;
                  fprintf fact_out "      return new %s(((JSONArray)o).%s(1));\n"
                    (List.assoc var_name names)
                    (get env t false);
           )
       | `Inherit _ -> assert false
    )
    vars;
  fprintf fact_out "    throw new JSONException(\"Invalid tag: \" + tag);\n";
  fprintf fact_out "  }\n";
  fprintf fact_out "}\n";
  close_out fact_out;
  env

(* Translate a record into a Java class.  Each record field becomes a field
 * within the class.
 *)
and trans_record my_name env (`Record (loc, fields, annots)) =
  (* Remove `Inherit values *)
  let fields = List.map
    (function
       | `Field _ as f -> f
       | `Inherit _ -> assert false
    )
    fields in
  (* Translate field types *)
  let (java_tys, env) = List.fold_left
    (fun (java_tys, env) -> function
       | `Field (_, (field_name, _, _), atd_ty) ->
           let (java_ty, env) = trans_inner env atd_ty in
           ((field_name, java_ty) :: java_tys, env)
    )
    ([], env) fields in
  let java_tys = List.rev java_tys in
  (* Output Java class *)
  let class_name = to_class_name my_name in
  let out = open_class env class_name in
  (* Javadoc *)
  output_string out (javadoc loc annots "");
  fprintf out "public class %s implements Atdj {\n" class_name;
  fprintf out "  /**\n";
  fprintf out "   * Construct from a JSON string.\n";
  fprintf out "   */\n";
  fprintf out "  public %s(String s) throws JSONException {\n" class_name;
  fprintf out "    this(new JSONObject(s));\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  %s(JSONObject jo) throws JSONException {\n" class_name;
  let env = List.fold_left
    (fun env (`Field (loc, (field_name, _, annots), _) as field) ->
       let cmd = assign_field env field (List.assoc field_name java_tys) in
       fprintf out "%s" cmd;
       env
    )
    env fields in
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public Visitor accept(Visitor v) {\n";
  fprintf out "    v.visit(this);\n";
  List.iter
    (fun (`Field (_, (field_name, _, _), atd_ty)) ->
       output_string out
         (accept env field_name (List.assoc field_name java_tys) atd_ty "v" "    ");
    )
    fields;
  fprintf out "  return v;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public String toString() {\n";
  fprintf out "    return toString(0);\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public String toString(int indent) {\n";
  fprintf out "    String str = \"\";\n";
  fprintf out "    str += Util.indent(indent) + \"{\\n\";\n";
  List.iter (fun field -> output_string out (to_string_field env field)) fields;
  fprintf out "    str = str.replaceAll(\",\\n$\", \"\\n\");\n";
  fprintf out "    str += Util.indent(indent) + \"}\";\n";
  fprintf out "    return str;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public int hashCode() {\n";
  fprintf out "    int h = 1;\n";
  List.iter
    (function `Field (_, (field_name, _, _), atd_ty) ->
       let java_ty = List.assoc field_name java_tys in
       fprintf out "    h *= 31;\n";
       output_string out (hash_code env atd_ty java_ty field_name "h" "    ")
    )
    fields;
  fprintf out "    return h;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /**\n";
  fprintf out "   * Comparison, using a total order.\n";
  fprintf out "   * @return The result of the comparison.\n";
  fprintf out "   */\n";
  fprintf out "  public int compareTo(%s that) {\n" class_name;
  fprintf out "    int cmp = 0;\n";
  List.iter
    (function `Field (_, (field_name, _, _), atd_ty) ->
       let java_ty = List.assoc field_name java_tys in
       output_string out
         (compare_to env atd_ty java_ty ("this." ^ field_name) ("that." ^ field_name) "cmp" "    ");
       fprintf out "    if (cmp != 0)\n";
       fprintf out "      return cmp;\n";
    )
    fields;
  fprintf out "    return 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /**\n";
  fprintf out "   * Test for equality.\n";
  fprintf out "   * @return Whether the two objects are equal.\n";
  fprintf out "   */\n";
  fprintf out "  public boolean equals(%s that) {\n" class_name;
  fprintf out "    return this.compareTo(that) == 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  List.iter
    (function `Field (loc, (field_name, _, annots), _) ->
       let java_ty = List.assoc field_name java_tys in
       output_string out (javadoc loc annots "  ");
       fprintf out "  public final %s %s;\n" java_ty field_name)
    fields;
  fprintf out "}\n";
  close_out out;
  { env with types = `Class (class_name, java_tys) :: env.types }

(* Translate an `inner' type i.e. a type that occurs within a record or sum *)
and trans_inner env atd_ty =
  match atd_ty with
  | `Option _ as opt ->
      trans_option env opt
  | `Name (_, (_, name1, _), _) ->
      (match norm_ty env atd_ty with
         | `Name (_, (_, name2, _), _) ->
             (* It's a primitive type e.g. int *)
             (to_class_name name2, env)
         | _ ->
             (to_class_name name1, env)
      )
  | `List (_, sub_atd_ty, _)  ->
      let (ty', env) = trans_inner env sub_atd_ty in
      (ty' ^ "[]", env)
  | _ -> assert false

(* Translate an option type *)
and trans_option env (`Option (_, atd_ty, _)) =
  let (java_ty, env) = trans_inner env atd_ty in
  let class_name = to_class_name (java_ty ^ "Opt") in
  let out = open_class env class_name in
  fprintf out "/** An optional %s. */\n" java_ty;
  fprintf out "public class %s implements Atdj {\n" class_name;
  fprintf out "  %s() {\n" class_name;
  fprintf out "    is_set = false;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /**\n";
  fprintf out "   * Construct from a JSON string.\n";
  fprintf out "   */\n";
  fprintf out "  public %s(String s) throws JSONException {\n" class_name;
  fprintf out "    try {\n";
  fprintf out "      init(new JSONArray(s));\n";
  fprintf out "    } catch (JSONException e) {\n";
  fprintf out "      init(s);\n";
  fprintf out "    }\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Construct an option from an org.json parsed value \n";
  fprintf out "  %s(Object value) throws JSONException {\n" class_name;
  fprintf out "    init(value);\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  void init(Object value) throws JSONException {\n";
  fprintf out "    if (Util.isNone(value))\n";
  fprintf out "      is_set = false;\n";
  fprintf out "    else if (Util.isSome(value)) {\n";
  fprintf out "      is_set = true;\n";
  output_string out (assign env "this.value"
                       (sprintf "((JSONArray)value).%s(1)" (get env atd_ty false))
                       java_ty atd_ty "      ");
  fprintf out "    } else\n";
  fprintf out "      throw new JSONException(\"Invalid option value\");\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public Visitor accept(Visitor v) {\n";
  (* Don't visit optional primitive types *)
  if not (List.mem class_name ["BoolOpt"; "IntOpt"; "FloatOpt"; "StringOpt"]) then (
    fprintf out "    v.visit(this);\n";
    if (can_accept env atd_ty) then (
      fprintf out "    if (is_set)\n";
      output_string out (accept env "value" java_ty atd_ty "v" "      ");
    );
  );
  fprintf out "    return v;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /** Attempt to get the value.\n";
  fprintf out "   *  @throws JSONException if the value is not set. */\n";
  fprintf out "  public %s get() throws JSONException {\n" java_ty;
  fprintf out "    if (is_set)\n";
  fprintf out "      return value;\n";
  fprintf out "    else\n";
  fprintf out "      throw new JSONException(\"Value is not set\");\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public String toString() {\n";
  fprintf out "    return toString(0);\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public String toString(int indent) {\n";
  fprintf out "    return toString(0, false);\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  String toString(int indent, boolean lift) {\n";
  fprintf out "    String str = \"\";\n";
  fprintf out "    if (is_set) {\n";
  fprintf out "      if (!lift)\n";
  fprintf out "        str += \"[\\\"Some\\\", \";\n";
  output_string out (to_string env "value" atd_ty false "        ");
  fprintf out "      if (!lift)\n";
  fprintf out "        str += \"]\";\n";
  fprintf out "    } else\n";
  fprintf out "      str = \"None\";\n";
  fprintf out "    return str;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  public int hashCode() {\n";
  fprintf out "    int h = 0;\n";
  fprintf out "    if (is_set)\n";
  output_string out (hash_code env atd_ty java_ty "value" "h" "    ");
  fprintf out "    return h;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /**\n";
  fprintf out "   * Comparison, using a total order.\n";
  fprintf out "   * @return The result of the comparison.\n";
  fprintf out "   */\n";
  fprintf out "  public int compareTo(%s that) {\n" class_name;
  fprintf out "    if (!this.is_set && !that.is_set)\n";
  fprintf out "      return 0;\n";
  fprintf out "    else if (!this.is_set && that.is_set)\n";
  fprintf out "      return -1;\n";
  fprintf out "    else if (this.is_set && !that.is_set)\n";
  fprintf out "      return 1;\n";
  fprintf out "    else {\n";
  fprintf out "      int cmp = 0;\n";
  output_string out
    (compare_to
       env
       atd_ty
       java_ty
       "this.value"
       "that.value"
       "cmp"
       "      "
    );
  fprintf out "      return cmp;\n";
  fprintf out "    }\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /**\n";
  fprintf out "   * Test for equality.\n";
  fprintf out "   * @return Whether the two objects are equal.\n";
  fprintf out "   */\n";
  fprintf out "  public boolean equals(%s that) {\n" class_name;
  fprintf out "    return this.compareTo(that) == 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  /** Whether the {@link #value} is set. */\n";
  fprintf out "  public boolean is_set;\n";
  fprintf out "  /** The value itself. */\n";
  fprintf out "  public %s value;\n" java_ty;
  fprintf out "}\n";
  close_out out;
  (class_name,
   { env with types = `Class (class_name, ["value", java_ty]) :: env.types } )


(* --------------------------------------------------------------------------- *)
(* Visitors *)

(* Remove duplicate elements from a list *)
let unique xs =
  let xs' = List.sort Pervasives.compare xs in
  let rec f = function
    | (y::z::zs) -> if y = z then f (z::zs) else y :: (f (z :: zs))
    | zs         -> zs in
  f xs'

(* Generate a Visitor interface that has a method for each accepting type.  We
 * also generate a `SimpleVisitor' class that implements the Visitor interface.
 * This class performs a no-op upon visiting each type.  However, it may be
 * sub-classed to perform more interesting actions at speicifc types.
 *)
let output_visitor env =
  let out = open_class env "Visitor" in
  fprintf out "/**\n";
  fprintf out " * The visitor interface.\n";
  fprintf out " */\n";
  fprintf out "public interface Visitor {\n";
  (* Don't visit optional primitive types *)
  let filtered = ["BoolOpt"; "IntOpt"; "FloatOpt"; "StringOpt"] in
  List.iter
    (function
       | `Class (name, _) ->
           if not (List.mem name filtered) then
             fprintf out "  public void visit(%s value);\n" name;
       | `Interface _ -> ()
    )
    (unique env.types);
  fprintf out "}\n";
  let out = open_class env "SimpleVisitor" in
  fprintf out "/**\n";
  fprintf out " * A no-op imlementation of the {@link Visitor} interface.\n";
  fprintf out " * Methods can be overriden by clients as required.\n";
  fprintf out " */\n";
  fprintf out "public class SimpleVisitor implements Visitor {\n";
  List.iter
    (function
       | `Class (name, _) ->
           if not (List.mem name filtered) then
             fprintf out "  public void visit(%s value) { }\n" name;
       | `Interface _ -> ()
    )
    (unique env.types);
  fprintf out "}\n"


(* --------------------------------------------------------------------------- *)
(* Graphical ouput *)

(* Ouput a dot graph of the class hierarchy *)
let output_graph env =
  let filename =
    match env.input_file with
      | None   -> assert false
      | Some x -> Filename.chop_extension x ^ ".dot" in
  let out = open_out filename in
  let env = { env with
                types     = unique env.types;
                sub_types = unique env.sub_types } in
  fprintf out "digraph \"G\" {\n";
  fprintf out "  rankdir=BT\n";
  List.iter
    (function
       | `Class (c, args) ->
           (match args with
              | [] -> fprintf out "  %s [shape=box]\n" c
              | _  ->
                  let args' = String.concat "|"
                    (List.map
                       (fun (x, t) ->
                          let is_array = Str.string_match (Str.regexp ".*\\[\\]$") t 0 in
                          let t' = Str.global_replace (Str.regexp "\\[\\]$") "" t in
                          (* For primitive types, append the type name to the field name *)
                          if List.mem t' ["int"; "IntOpt";
                                          "boolean"; "BooleanOpt";
                                          "String"; "StringOpt";
                                          "double"; "DoubleOpt"] then
                            sprintf "%s:%s" x t
                          else (
                            (* For the rest, place an arc from the type to the field name,
                             * reversing the apparent direction to give the expected view *)
                            fprintf out "  \"%s\" -> \"%s\":%s [style=dashed,dir=back]\n" t' c x;
                            (* The <> notation labels a node *)
                            sprintf "<%s>%s%s" x x (if is_array then ":[]" else "")
                          )
                       )
                       args) in
                  fprintf out "  %s [shape=record, label=\"{\\N|{%s}}\"]\n" c args'
           )
       | `Interface i -> fprintf out "  %s [shape=oval]\n" i
    )
    env.types;
  List.iter
    (function (ty, ty') -> fprintf out "  %s -> %s [style=solid]\n" ty ty')
    env.sub_types;
  fprintf out "}\n";
  close_out out


(* --------------------------------------------------------------------------- *)
(* Helper classes *)

let output_atdj env =
  let out = open_class env "Atdj" in
  fprintf out "/**\n";
  fprintf out " * Common utility interface.\n";
  fprintf out " */\n";
  fprintf out "public interface Atdj {\n";
  fprintf out "  /**\n";
  fprintf out "   * Get the JSON string representation.\n";
  fprintf out "   * @return The JSON string.\n";
  fprintf out "   */\n";
  fprintf out "  String toString();\n";
  fprintf out "  /**\n";
  fprintf out "   * Get the JSON string representation, with each line indented.\n";
  fprintf out "   * @param indent The number of spaces to indent by.\n";
  fprintf out "   * @return The indented JSON string.\n";
  fprintf out "   */\n";
  fprintf out "  String toString(int indent);\n";
  fprintf out "  /**\n";
  fprintf out "   * Get the hash code.\n";
  fprintf out "   * @return The hash code.\n";
  fprintf out "   */\n";
  fprintf out "  int hashCode();\n";
  fprintf out "  /**\n";
  fprintf out "   * Accept a visitor.\n";
  fprintf out "   * @param visitor The visitor.\n";
  fprintf out "   * @return The same visitor, for covenience.\n";
  fprintf out "   */\n";
  fprintf out "  Visitor accept(Visitor visitor);\n";
  fprintf out "}\n";
  close_out out

let output_util env =
  let out = open_class env "Util" in
  fprintf out "class Util {\n";
  fprintf out "  static String indent(int n) {\n";
  fprintf out "    String str = \"\";\n";
  fprintf out "    for (int i = 0; i < n; ++i)\n";
  fprintf out "       str += \" \";\n";
  fprintf out "    return str;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  static int compareTo(boolean[] xs, boolean[] ys) {\n";
  fprintf out "    int minLen = Math.min(xs.length, ys.length);\n";
  fprintf out "    for (int i = 0; i < minLen; ++i) {\n";
  fprintf out "      int cmp = new Boolean(xs[i]).compareTo(new Boolean(ys[i]));\n";
  fprintf out "      if (cmp != 0)\n";
  fprintf out "        return cmp;\n";
  fprintf out "    }\n";
  fprintf out "    if (xs.length < ys.length)\n";
  fprintf out "      return -1;\n";
  fprintf out "    else if (xs.length > ys.length)\n";
  fprintf out "      return 1;\n";
  fprintf out "    else\n";
  fprintf out "      return 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  static int compareTo(int[] xs, int[] ys) {\n";
  fprintf out "    int minLen = Math.min(xs.length, ys.length);\n";
  fprintf out "    for (int i = 0; i < minLen; ++i) {\n";
  fprintf out "      int cmp = new Integer(xs[i]).compareTo(new Integer(ys[i]));\n";
  fprintf out "      if (cmp != 0)\n";
  fprintf out "        return cmp;\n";
  fprintf out "    }\n";
  fprintf out "    if (xs.length < ys.length)\n";
  fprintf out "      return -1;\n";
  fprintf out "    else if (xs.length > ys.length)\n";
  fprintf out "      return 1;\n";
  fprintf out "    else\n";
  fprintf out "      return 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  static int compareTo(double[] xs, double[] ys) {\n";
  fprintf out "    int minLen = Math.min(xs.length, ys.length);\n";
  fprintf out "    for (int i = 0; i < minLen; ++i) {\n";
  fprintf out "      int cmp = new Double(xs[i]).compareTo(new Double(ys[i]));\n";
  fprintf out "      if (cmp != 0)\n";
  fprintf out "        return cmp;\n";
  fprintf out "    }\n";
  fprintf out "    if (xs.length < ys.length)\n";
  fprintf out "      return -1;\n";
  fprintf out "    else if (xs.length > ys.length)\n";
  fprintf out "      return 1;\n";
  fprintf out "    else\n";
  fprintf out "      return 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  static int compareTo(String[] xs, String[] ys) {\n";
  fprintf out "    int minLen = Math.min(xs.length, ys.length);\n";
  fprintf out "    for (int i = 0; i < minLen; ++i) {\n";
  fprintf out "      int cmp = xs[i].compareTo(ys[i]);\n";
  fprintf out "      if (cmp != 0)\n";
  fprintf out "        return cmp;\n";
  fprintf out "    }\n";
  fprintf out "    if (xs.length < ys.length)\n";
  fprintf out "      return -1;\n";
  fprintf out "    else if (xs.length > ys.length)\n";
  fprintf out "      return 1;\n";
  fprintf out "    else\n";
  fprintf out "      return 0;\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Extract the tag of sum-typed value\n";
  fprintf out "  static String tag(Object value) throws JSONException {\n";
  fprintf out "    if (value instanceof String)\n";
  fprintf out "      return (String)value;\n";
  fprintf out "    else if (value instanceof JSONArray)\n";
  fprintf out "      return ((JSONArray)value).getString(0);\n";
  fprintf out "    else throw new JSONException(\"Cannot extract type\");\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Is an option value a none?\n";
  fprintf out "  static boolean isNone(Object value) throws JSONException {\n";
  fprintf out "    return (value instanceof String) && (((String)value).equals(\"None\"));\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Is an option value a Some?\n";
  fprintf out "  static boolean isSome(Object value) throws JSONException {\n";
  fprintf out "    return (value instanceof JSONArray)\n";
  fprintf out "      && ((JSONArray)value).getString(0).equals(\"Some\");\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Escape double quotes and backslashes\n";
  fprintf out "  static String escape(String str) {\n";
  fprintf out "    return str.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\");\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Parse a JSON string, strictly\n";
  fprintf out "  static String parseJSONString(String str) throws JSONException {\n";
  fprintf out "    if (str.length() < 1 || str.charAt(0) != '\"')\n";
  fprintf out "      // throw new JSONException(\"Expected '\\\"', line 0, column 0\");\n";
  fprintf out "      throw new JSONException(\"Expected '\\\"'\");\n";
  fprintf out "    for (int i = 1; i < str.length(); ++i)\n";
  fprintf out "      if (str.charAt(i) == '\"' && str.charAt(i - 1) != '\\\\' && i < str.length() - 1)\n";
  fprintf out "        // throw new JSONException(\"Trailing characters, line 0, column \" + (i + 1));\n";
  fprintf out "        throw new JSONException(\"Trailing characters\");\n";
  fprintf out "    if (str.length() < 2 || str.charAt(str.length() - 1) != '\"')\n";
  fprintf out "      // throw new JSONException(\"Unterminated string '\\\"', line 0, column \" + (str.length() - 1));\n";
  fprintf out "      throw new JSONException(\"Unterminated string '\\\"'\");\n";
  fprintf out "    return str.substring(1, str.length() - 1);\n";
  fprintf out "  }\n";
  fprintf out "\n";
  fprintf out "  // Unescape escaped backslashes and double quotations.\n";
  fprintf out "  // All other escape sequences are considered invalid (this is probably too strict).\n";
  fprintf out "  static String unescapeString(String str) throws JSONException {\n";
  fprintf out "    StringBuffer buf = new StringBuffer();\n";
  fprintf out "    for (int i = 0; i < str.length(); ++i) {\n";
  fprintf out "      if (str.charAt(i) == '\\\\') {\n";
  fprintf out "        if (i == str.length() - 1 ||\n";
  fprintf out "            (str.charAt(i + 1) != '\\\\' && str.charAt(i + 1) != '\"'))\n";
  fprintf out "          // throw new JSONException(\"Invalid escape, line 0, column \" + (i + 1));\n";
  fprintf out "          throw new JSONException(\"Invalid escape\");\n";
  fprintf out "        else {\n";
  fprintf out "          buf.append(str.charAt(i + 1));\n";
  fprintf out "          ++i;\n";
  fprintf out "        }\n";
  fprintf out "      } else {\n";
  fprintf out "        buf.append(str.charAt(i));\n";
  fprintf out "      }\n";
  fprintf out "    }\n";
  fprintf out "    return buf.toString();\n";
  fprintf out "  }\n";
  fprintf out "}\n";
  close_out out

let output_package_javadoc env (loc, annots) =
  let out = open_out (env.package_dir ^ "/" ^ "package.html") in
  output_string out "<body>\n";
  let from_doc_para acc para =
    List.fold_left
      (fun acc -> function
         | `Text text -> text :: acc
         | `Code _    -> failwith "Not yet implemented"
      )
      acc
      para in
  let from_doc = function
    | `Text blocks ->
        List.fold_left
          (fun acc -> function
             | `Paragraph para -> from_doc_para acc para
             | `Pre _          -> failwith "Not yet implemented"
          )
          []
          blocks in
  (match Ag_doc.get_doc loc annots with
     | Some doc ->
         let str = String.concat "\n<p>\n" (List.rev (from_doc doc)) in
         output_string out str
     | _ -> ()
  );
  output_string out "\n</body>";
  close_out out


(* --------------------------------------------------------------------------- *)
(* Main *)

let args_spec env = Arg.align
    [ ("-package",
       Arg.String (fun x -> env := { !env with package = x }),
       " Package name of generated files")
    ; ("-graph",
       Arg.Unit   (fun x -> env := { !env with graph = true }),
       " Output class graph in dot format")
    ]

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> <file>\nOptions are:"

let make_package_dirs package =
  let re   = Str.regexp "\\." in
  let dirs = Str.split re package in
  List.fold_left
    (fun parent dir ->
       let full_dir = parent ^ "/" ^ dir in
       if Sys.file_exists full_dir then
         if not (Sys.is_directory full_dir) then
           failwith (
             sprintf "Cannot make directory %s: file already exists" full_dir
           )
         else ()
       else
         Unix.mkdir full_dir 0o755;
       full_dir
    )
    "." dirs

let () =
  let env = ref default_env in

  (* Parse command line options *)
  let args_spec' = args_spec env in
  Arg.parse args_spec'
    (fun x -> env := { !env with input_file = Some x }) usage_msg;

  (* Check for input file *)
  let input_file =
    match !env.input_file with
      | None   ->
          prerr_endline "No input file specified";
          Arg.usage args_spec' usage_msg;
          exit 1
      | Some x ->
          if not (Filename.check_suffix x ".atd") then (
            prerr_endline "Input filename must end with `.atd'";
            Arg.usage args_spec' usage_msg;
            exit 1
          ) else x in

  (* Validate package name *)
  let re = Str.regexp "^[a-zA-Z0-9]+\\(\\.[a-zA-Z0-9]+\\)*$" in
  if not (Str.string_match re !env.package 0) then (
    prerr_endline "Invalid package name";
    Arg.usage args_spec' usage_msg;
    exit 1
  );

  (* Parse ATD file *)
  let head, atd_module =
    Atd_util.load_file
      ~expand:false ~inherit_fields:true ~inherit_variants:true input_file
  in
  env := { !env with
             module_items =
               List.map
                 (function (`Type (_, (name, _, _), atd_ty)) -> (name, atd_ty))
                 atd_module };

  (* Create package directories *)
  env := { !env with package_dir = make_package_dirs !env.package };

  (* Pre-populate fresh name generator with top-level names.
   * Top-level names are guaranteed unique amonst themselves, and
   * therefore can be output without freshening.  However, variant
   * names may collide, both with top-level names and also amongst
   * themselves.  Therefore these latter names must be freshened (see
   * trans_variant).
   *)
  let env =
    List.fold_left
      (fun env (`Type (_, (name, _, _), _)) ->
         let (env, _) = freshen env (to_class_name name) in
         (* Bodge to prevent possible collision between variants and ... *)
         let (env, _) = freshen env (to_class_name (name ^ "Opt")) in
         let (env, _) = freshen env (to_class_name (name ^ "Factory")) in
         env)
      !env
      atd_module in

  (* Generate classes from ATD definition *)
  let env = trans_module env atd_module in

  (* Generate the visitor interface *)
  output_visitor env;

  (* Output helper classes *)
  output_util env;
  output_atdj env;

  output_package_javadoc env head;

  (* Output graph in dot format *)
  if env.graph then
    output_graph env
