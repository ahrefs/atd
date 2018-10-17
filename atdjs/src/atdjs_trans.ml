open Atd.Import
open Atdjs_names
open Atdjs_env
open Atdjs_util

(* Declare a case class field, with support for optional fields.  The are two
 * kinds of optional fields: `With_default (~) and `Optional (?).  For both
 * kinds, we return the following values if the field is absent:
 *
 *   bool   -> false
 *   int    -> 0
 *   float  -> 0.0
 *   string -> ""
 *   list   -> Nil
 *   option -> None
 *
 * Optional fields of record and sum types are not supported. They are
 * treated as required fields.
 *
 * Fields of the `Optional kind extend this behaviour by automatically lifting
 * values of type t to option t by wrapping within a `Some'.
 * Hence `Optional may only be applied to fields of type option t.
 * Note that absent fields are still
 * assigned `None', as before.
 *
 * For `With_default fields, of types bool, int, float, string and list, we use
 * the org.json opt methods to extract the field.  These methods already return
 * the appropriate defaults if field is absent.  For option types, we manually
 * check for the field and manually create a default.  If the field is present,
 * then we wrap its values as necessary.
*)
let declare_field env
    (`Field (_, (atd_field_name, kind, annots), atd_ty)) scala_ty =
  let field_name = get_js_field_name atd_field_name annots in
  let opt_default =
    match kind with
    | Atd.Ast.With_default ->
        (match norm_ty env atd_ty with
         | Name (_, (_, name, _), _) ->
             (match name with
              | "bool" -> Some "false"
              | "int" -> Some "0"
              | "float" -> Some "0.0"
              | "string" -> Some "\"\""
              | _ -> None (* TODO: fail if no default is provided *)
             )
         | List _ ->
            Some "Nil"
         | Option _ -> Some "None"
         | _ ->
             None (* TODO: fail if no default is provided *)
        )
    | _ ->
        None
  in
  let opt_set_default = match opt_default with Some s -> " = "^s | None -> "" in
  sprintf "  %s : %s%s" field_name scala_ty opt_set_default


(* Generate an argonaut field for a record field. *)
let to_string_field env = function
  | (`Field (_, (atd_field_name, kind, annots), atd_ty)) ->
      let json_field_name = get_json_field_name atd_field_name annots in
      let field_name = get_js_field_name atd_field_name annots in

      (* TODO: Omit fields with default value. *)
      sprintf "    \"%s\" := %s,\n" json_field_name field_name

(* Generate a javadoc comment *)
let javadoc loc annots indent =
  let from_inline_text text = indent ^ " * " ^ text ^ "\n" in
  (* Assume that code is the name of a field that is defined
     in the same class *)
  let from_inline_code code = indent ^ " * {@link #" ^ code ^ "}\n" in
  let from_doc_para =
    List.fold_left (fun acc -> function
      | Atd.Doc.Text text -> (from_inline_text text) :: acc
      | Code code -> (from_inline_code code) :: acc
    ) in
  let from_doc =
    List.fold_left (fun acc -> function
      | Atd.Doc.Paragraph para -> from_doc_para acc para
      | Pre _ -> failwith "Preformatted doc blocks are not supported"
    ) []
  in
  (match Atd.Doc.get_doc loc annots with
   | Some doc ->
       let header = indent ^ "/**\n" in
       let footer = indent ^ " */\n" in
       let body   =
         String.concat "" (List.rev (from_doc doc)) in
       header ^ body ^ footer
   | None     -> ""
  )


(* ------------------------------------------------------------------------- *)
(* Translation of ATD types into Scala types *)

(* For sum and record types, we generate a Scala case classes.
 *
 * Each record is translated to a single case class with an argument/member
 * for each field.
 *
 * Each sum type is translated into a sealed abstract class with
 * a concrete case class extending it for each variant of the sum type.
 *
 * All the classes generated extend Atdjs and can be encoded into JSON via the
 * argonaut encoder at Atdjs.argonautCodecAtdjs.
 *
 * Decoding JSON is not yet supported.
 *
 * For types bool, int, float, string, list, and option, we use existing
 * Scala types. We do generate type aliases when they are used in type aliases.
 * They can be encoded to (or decoded from) JSON via the standard argonaut codecs.
*)

let open_package env =
  let out = env.output in
  fprintf out "\
// @flow
// Automatically generated; do not edit

";
  fun () -> output_string out "\n//So long and thanks for all the fish\\\\\n"

let rec trans_module env items = List.fold_left trans_outer env items

and trans_outer env (Atd.Ast.Type (_, (name, _, annots), atd_ty)) =
  match unwrap atd_ty with
  (*| Sum (loc, v, a) ->
      trans_sum name env (loc, v, a)*)
  | Record _ | Name _ | Tuple _ | List _ ->
      trans_alias name env annots atd_ty
  | x -> type_not_supported x

(* Translation of sum types.  For a sum type
 *
 *   type ty = Foo | Bar of whatever
 *
 * Flow currently supports disjoint unions of object types, which we could use
 * for sum types. https://flow.org/en/docs/types/unions/#toc-disjoint-unions
 *
 * A nicer approach would be to use disjoint unions of tuple types, as
 * the JS representation would then match the JSON representation, but they are
 * not yet supported in Flow. https://github.com/facebook/flow/issues/4296
*)
and trans_sum my_name env (loc, vars, annots) =
  type_not_supported (Sum(loc, vars, annots))

(* Translate a record into a Java class.  Each record field becomes a field
 * within the class.
*)
and trans_record env (loc, fields, annots) =
  (* Remove `Inherit values *)
  let fields = List.map
      (function
        | `Field _ as f -> f
        | `Inherit _ -> assert false
      )
      fields in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "{\n";
  List.iter (function
      | `Field (_, (field_name, kind, annots), atd_ty) ->
         let field_name = get_js_field_name field_name annots in
         let flow_ty = trans_inner env atd_ty in
         let opt = if kind == Atd.Ast.Optional then "?" else "" in
         bprintf buf "  %s%s: %s,\n" field_name opt flow_ty
  ) fields;
  Buffer.add_string buf "}";
  Buffer.contents buf

and trans_alias name env annots ty =
  let scala_name = Atdjs_names.get_flow_type_name name annots in
  fprintf env.output "\ntype %s = %s;\n" scala_name (trans_inner env ty);
  env

(* Translate an `inner' type i.e. a type that occurs within a record or sum *)
and trans_inner env atd_ty =
  match atd_ty with
  | Name (_, (_, name1, _), _) ->
      (match norm_ty env atd_ty with
       | Name (_, (_, name2, _), _) ->
           (* It's a primitive type e.g. int *)
           Atdjs_names.to_flow_name name2
       | _ ->
           Atdjs_names.to_flow_name name1
      )
  | List (_, sub_atd_ty, _)  ->
      let ty' = trans_inner env sub_atd_ty in
      "Array<" ^ ty' ^ ">"
  | Option (_, sub_atd_ty, _) ->
      let ty' = trans_inner env sub_atd_ty in
      "?" ^ ty'
  | Tuple (_, cells, annot) ->
     let cells = List.map (fun (_, ty, _) -> trans_inner env ty) cells in
     sprintf "[%s]" (String.concat ", " cells)
  | Record (loc, v, a) ->
      trans_record env (loc, v, a)
  | x -> type_not_supported x
