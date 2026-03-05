(*
   OCaml code generation for JSON support using the Yojson AST.

   For each ATD type 'foo', the generated code contains:
   - A type definition 'type foo = ...'
   - A creation function 'let make_foo ...' (for record types only)
   - A deserialization function 'let foo_of_yojson ...'
   - A serialization function 'let yojson_of_foo ...'
   - Top-level I/O functions

   Two files are produced: 'foo.ml' and 'foo.mli'.

   Design notes:
   - Uses Yojson.Safe.t as the intermediate JSON representation,
     not the semi-secret streaming parser functions used by atdgen.
   - Generates classic OCaml variants by default; use <ocaml repr="poly">
     on the sum type to get polymorphic variants instead.
   - No separate _t.ml / _j.ml split: one ATD file → one OCaml module.
   - Naming convention: foo_of_yojson / yojson_of_foo (same as
     ppx_yojson_conv, compatible with ppx_deriving.yojson).
   - ATD type names that are OCaml keywords or conflict with the naming
     scheme ('json', 'yojson') are automatically renamed with a trailing '_'
     (or '_2', '_3', … if needed to avoid conflicts). See init_env.
*)

open Printf
open Atd.Ast
module A = Atd.Ast
module B = Indent

(* List.concat_map was added in OCaml 4.10; provide a compatible version. *)
let concat_map f l = List.concat (List.map f l)

(* ============ Annotation schema ============ *)

let annot_schema_ocaml : Atd.Annot.schema_section = {
  section = "ocaml";
  fields = [
    Type_def,  "attr";    (* <ocaml attr="..."> on a type def: append [@@...] *)
    Type_expr, "repr";    (* <ocaml repr="poly"> on a sum type *)
    Type_expr, "module";  (* <ocaml module="M"> on a wrap: use M.t/M.wrap/M.unwrap *)
    Type_expr, "t";       (* <ocaml t="..."> on a wrap: explicit OCaml type *)
    Type_expr, "wrap";    (* <ocaml wrap="..."> on a wrap: deserialize function *)
    Type_expr, "unwrap";  (* <ocaml unwrap="..."> on a wrap: serialize function *)
    Variant, "name";      (* <ocaml name="..."> on a variant constructor *)
    Field, "default";     (* <ocaml default="..."> on a with-default field *)
  ]
}

let annot_schema : Atd.Annot.schema =
  annot_schema_ocaml :: (Atd.Json.annot_schema_json @ Atd.Doc.annot_schema)

(* ============ Errors ============ *)

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdml: " ^ msg)

(* ============ Identifier translation ============ *)

(*
   All OCaml keywords. Type names matching these cannot appear as-is in
   generated OCaml code. See init_env for how conflicts are resolved.
*)
let ocaml_keywords = [
  "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do";
  "done"; "downto"; "else"; "end"; "exception"; "external"; "false";
  "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit";
  "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor";
  "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec";
  "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct";
  "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when";
  "while"; "with";
]

(*
   Names that conflict with our function naming scheme:
   - 'yojson' would yield both 'yojson_of_yojson' (serializer) and
     'yojson_of_yojson' (deserializer) — identical names.
   - 'json' would yield both 'json_of_json' — same problem.
*)
let naming_conflicts = ["yojson"; "json"]

(*
   Build a translate function that maps each ATD type name in [defs] to a
   unique, valid OCaml identifier. The mapping is stable (idempotent) and
   consistent across calls with the same inputs:
   - Reserved names (keywords, naming conflicts) get a trailing '_' appended,
     or '_2', '_3', … if there are further conflicts.
   - All other names are returned unchanged.
   - Pre-registers all names in definition order to resolve clashes
     deterministically.
*)
let init_env (defs : A.type_def list) : string -> string =
  let registry =
    Atd.Unique_name.init
      ~reserved_identifiers:(ocaml_keywords @ naming_conflicts)
      ~reserved_prefixes:[]
      ~safe_prefix:"x_"
  in
  (* Register all type names upfront so conflict resolution is deterministic
     regardless of where in the file each name is first referenced. *)
  List.iter (fun (_, (name, _, _), _) ->
    ignore (Atd.Unique_name.translate registry name)
  ) defs;
  Atd.Unique_name.translate registry

(*
   Build a name translator for a local scope (record fields or variant
   constructors).  Any name that conflicts with an OCaml keyword is renamed
   by appending '_', '_2', '_3', … as needed to avoid collisions with other
   names in the same scope.
   All names must be supplied upfront so that conflict resolution is
   deterministic regardless of ordering.
*)
let make_local_env names =
  let registry =
    Atd.Unique_name.init
      ~reserved_identifiers:ocaml_keywords
      ~reserved_prefixes:[]
      ~safe_prefix:"x_"
  in
  List.iter (fun n -> ignore (Atd.Unique_name.translate registry n)) names;
  Atd.Unique_name.translate registry

(* ============ Annotation helpers ============ *)

(* Get <ocaml default="..."> for with-default fields *)
let get_ocaml_default an =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["ocaml"]
    ~field:"default"
    an

(* Get <ocaml repr="poly"> to request polymorphic variants *)
let get_ocaml_repr an =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["ocaml"]
    ~field:"repr"
    an

(* Get <ocaml attr="..."> for a type definition; value is placed inside [@@...] *)
let get_ocaml_attr an =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["ocaml"]
    ~field:"attr"
    an

(* Get wrap-related annotations for a 'wrap' type expression.
   Supports:
     <ocaml module="M">              → uses M.t, M.wrap, M.unwrap
     <ocaml module="M" t="T">       → M.wrap, M.unwrap; explicit type T
     <ocaml t="T" wrap="f" unwrap="g">  → fully explicit
   Any field not specified defaults to None (identity / inner type). *)
let get_ocaml_wrap an =
  let get field =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["ocaml"]
      ~field
      an
  in
  let module_name = get "module" in
  let from_module suffix = Option.map (fun m -> m ^ "." ^ suffix) module_name in
  let wrap_t    = match get "t"      with Some v -> Some v | None -> from_module "t" in
  let wrap_fn   = match get "wrap"   with Some v -> Some v | None -> from_module "wrap" in
  let unwrap_fn = match get "unwrap" with Some v -> Some v | None -> from_module "unwrap" in
  (wrap_t, wrap_fn, unwrap_fn)

(* Extract (normalize_expr, restore_expr) from an ocaml_adapter, if present. *)
let adapter_exprs (adapter : Atd.Json.json_adapter) =
  match adapter.ocaml_adapter with
  | None -> (None, None)
  | Some a -> (Some a.normalize, Some a.restore)

(* ============ Doc comment helpers ============ *)

(* Escape special characters for OCaml's ocamldoc format *)
let ocamldoc_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    Buffer.add_string buf (match c with
      | '{' -> "\\{" | '}' -> "\\}" | '[' -> "\\[" | ']' -> "\\]"
      | '@' -> "\\@" | '\\' -> "\\\\" | c -> String.make 1 c)
  ) s;
  Buffer.contents buf

let render_doc_inline = function
  | Atd.Doc.Text s -> ocamldoc_escape s
  | Atd.Doc.Code s -> "[" ^ ocamldoc_escape s ^ "]"

(* Render a doc block to a list of strings (lines of comment body) *)
let render_doc_block = function
  | Atd.Doc.Paragraph inlines ->
      let text = String.concat "" (List.map render_doc_inline inlines) in
      Atd.Doc.rewrap_paragraph ~max_length:70 text
  | Atd.Doc.Pre lines ->
      "{v" :: List.map (sprintf "  %s") lines @ ["v}"]

(* Convert a doc to a flat list of content lines (without the (** and *)) *)
let doc_content_lines doc =
  List.concat (List.mapi (fun i block ->
    (if i = 0 then [] else [""])
    @ render_doc_block block
  ) doc)

(* Generate a (** ... *) comment as B.t given body lines *)
let ocamldoc_comment_block (lines : string list) : B.t =
  match lines with
  | [] -> []
  | [line] -> [B.Line (sprintf "(** %s *)" line)]
  | _ ->
      [B.Line "(**"]
      @ List.map (fun l ->
          if l = "" then B.Line ""
          else B.Line ("   " ^ l)
        ) lines
      @ [B.Line "*)"]

(* Return B.t lines to prepend before a declaration, from a doc annotation *)
let doc_comment_prepend loc an : B.t =
  match Atd.Doc.get_doc loc an with
  | None -> []
  | Some doc -> ocamldoc_comment_block (doc_content_lines doc)

(* Wrap a declaration line with an optional trailing inline doc comment *)
let with_inline_doc decl_str loc an : B.t =
  match Atd.Doc.get_doc loc an with
  | None -> [B.Line decl_str]
  | Some doc ->
      match doc_content_lines doc with
      | [] -> [B.Line decl_str]
      | [one_line] -> [B.Line (sprintf "%s  (** %s *)" decl_str one_line)]
      | lines -> B.Line decl_str :: ocamldoc_comment_block lines

(* Get <ocaml name="..."> to rename OCaml identifiers *)
let get_ocaml_name default_name an =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default_name
    ~sections:["ocaml"]
    ~field:"name"
    an

(* ============ Naming conventions ============ *)

let of_yojson_name name = name ^ "_of_yojson"
let yojson_of_name name = "yojson_of_" ^ name
let of_json_name name = name ^ "_of_json"
let json_of_name name = "json_of_" ^ name
let make_name name = "make_" ^ name
let module_name name = String.capitalize_ascii name

(* Function parameter name for reading/writing a type variable *)
let tvar_reader v = "of_yojson_" ^ v
let tvar_writer v = "yojson_of_" ^ v

(* ============ Builtin OCaml type names ============ *)

let is_atd_builtin = function
  | "unit" | "bool" | "int" | "float" | "string" | "abstract" -> true
  | _ -> false

let builtin_ocaml_name = function
  | "unit" -> "unit"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "float"
  | "string" -> "string"
  | "abstract" -> "Yojson.Safe.t"
  | name -> name

(* ============ OCaml type expression from ATD type ============ *)

(*
   [tr] is the type-name translation function from init_env. It maps ATD
   type names to the OCaml identifiers used in the generated code.
*)
let rec type_expr_str tr (e : type_expr) : string =
  match e with
  | Sum _ | Record _ -> failwith "inline types are not supported"
  | Tuple (_, cells, _) ->
      let parts = List.map (fun (_, e, _) -> type_expr_str tr e) cells in
      "(" ^ String.concat " * " parts ^ ")"
  | List (_, e, _) -> type_expr_str tr e ^ " list"
  | Option (_, e, _) -> type_expr_str tr e ^ " option"
  | Nullable (_, e, _) -> type_expr_str tr e ^ " option"
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let (wrap_t, _, _) = get_ocaml_wrap an in
      (match wrap_t with
      | Some t -> t
      | None -> type_expr_str tr inner_e)
  | Name (_, (_, name, []), _) ->
      if is_atd_builtin name then builtin_ocaml_name name else tr name
  | Name (_, (_, name, params), _) ->
      let pstrs = List.map (type_expr_str tr) params in
      "(" ^ String.concat ", " pstrs ^ ") " ^ tr name
  | Tvar (_, v) -> "'" ^ v

(* ============ Type parameter formatting ============ *)

(* Format type parameter list for a type definition header, e.g. "('a, 'b) " *)
let type_params_str params =
  match params with
  | [] -> ""
  | [v] -> sprintf "'%s " v
  | vs -> sprintf "(%s) " (String.concat ", " (List.map (fun v -> "'" ^ v) vs))

(* Full type reference including parameters, e.g. "('a, 'b) pair" *)
let full_type_name name params =
  type_params_str params ^ name

(* ============ Reader function expressions ============ *)
(*
   Returns an OCaml expression of type (Yojson.Safe.t -> 'a).
   These are used as arguments to combinators like list_of_yojson.
*)

let rec reader_expr tr (e : type_expr) : string =
  match e with
  | Name (_, (_, "unit", []), _) -> "Atdml_runtime.unit_of_yojson"
  | Name (_, (_, "bool", []), _) -> "Atdml_runtime.bool_of_yojson"
  | Name (_, (_, "int", []), _) -> "Atdml_runtime.int_of_yojson"
  | Name (_, (_, "float", []), _) -> "Atdml_runtime.float_of_yojson"
  | Name (_, (_, "string", []), _) -> "Atdml_runtime.string_of_yojson"
  | Name (_, (_, "abstract", []), _) -> "(fun x -> x)"
  | Name (_, (_, name, []), _) -> of_yojson_name (tr name)
  | Name (_, (_, name, params), _) ->
      let readers = List.map (reader_expr tr) params in
      sprintf "(%s %s)" (of_yojson_name (tr name)) (String.concat " " readers)
  | List (_, e, _) ->
      sprintf "(Atdml_runtime.list_of_yojson %s)" (reader_expr tr e)
  | Option (_, e, _) ->
      sprintf "(Atdml_runtime.option_of_yojson %s)" (reader_expr tr e)
  | Nullable (_, e, _) ->
      sprintf "(Atdml_runtime.nullable_of_yojson %s)" (reader_expr tr e)
  | Tuple (_, cells, _) ->
      let n = List.length cells in
      let reads =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s (List.nth lst %d)" (reader_expr tr e) i
        ) cells
      in
      sprintf
        "(fun x -> match x with \
         | `List lst when List.length lst = %d -> (%s) \
         | _ -> Atdml_runtime.bad_type \"tuple\" x)"
        n
        (String.concat ", " reads)
  | Tvar (_, v) -> tvar_reader v
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let inner = reader_expr tr inner_e in
      let (_, wrap_fn, _) = get_ocaml_wrap an in
      (match wrap_fn with
      | None -> inner
      | Some f -> sprintf "(fun x -> (%s) (%s x))" f inner)
  | Sum _ | Record _ -> failwith "inline types are not supported"

(* ============ Writer function expressions ============ *)
(*
   Returns an OCaml expression of type ('a -> Yojson.Safe.t).
   These are used as arguments to combinators like yojson_of_list.
*)

let rec writer_expr tr (e : type_expr) : string =
  match e with
  | Name (_, (_, "unit", []), _) -> "Atdml_runtime.yojson_of_unit"
  | Name (_, (_, "bool", []), _) -> "Atdml_runtime.yojson_of_bool"
  | Name (_, (_, "int", []), _) -> "Atdml_runtime.yojson_of_int"
  | Name (_, (_, "float", []), _) -> "Atdml_runtime.yojson_of_float"
  | Name (_, (_, "string", []), _) -> "Atdml_runtime.yojson_of_string"
  | Name (_, (_, "abstract", []), _) -> "(fun x -> x)"
  | Name (_, (_, name, []), _) -> yojson_of_name (tr name)
  | Name (_, (_, name, params), _) ->
      let writers = List.map (writer_expr tr) params in
      sprintf "(%s %s)" (yojson_of_name (tr name)) (String.concat " " writers)
  | List (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_list %s)" (writer_expr tr e)
  | Option (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_option %s)" (writer_expr tr e)
  | Nullable (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_nullable %s)" (writer_expr tr e)
  | Tuple (_, cells, _) ->
      let vars = List.mapi (fun i _ -> sprintf "x%d" i) cells in
      let writes =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s %s" (writer_expr tr e) (List.nth vars i)
        ) cells
      in
      sprintf "(fun (%s) -> `List [%s])"
        (String.concat ", " vars)
        (String.concat "; " writes)
  | Tvar (_, v) -> tvar_writer v
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let inner = writer_expr tr inner_e in
      let (_, _, unwrap_fn) = get_ocaml_wrap an in
      (match unwrap_fn with
      | None -> inner
      | Some f -> sprintf "(fun x -> %s ((%s) x))" inner f)
  | Sum _ | Record _ -> failwith "inline types are not supported"

(* ============ Flatten variants ============ *)

let flatten_variants variants =
  List.map (fun (x : variant) ->
    match x with
    | Variant (loc, (orig_name, an), opt_e) -> (loc, orig_name, an, opt_e)
    | Inherit _ -> assert false
  ) variants

(* ============ Inlined runtime module ============ *)
(*
   Emitted verbatim at the top of every generated .ml file so that the
   generated code has no dependency on an external atdml-runtime library.
*)

let runtime_module : B.t =
  [
    B.Line {|(* Inlined runtime — no external dependency needed. *)
module Atdml_runtime = struct
  let bad_type expected_type x =
    Printf.ksprintf failwith "expected %s, got: %s"
      expected_type (Yojson.Safe.to_string x)

  let bad_sum type_name x =
    Printf.ksprintf failwith "invalid variant for type '%s': %s"
      type_name (Yojson.Safe.to_string x)

  let missing_field type_name field_name =
    Printf.ksprintf failwith "missing field '%s' in object of type '%s'"
      field_name type_name

  let bool_of_yojson = function
    | `Bool b -> b
    | x -> bad_type "bool" x

  let yojson_of_bool b = `Bool b

  let int_of_yojson = function
    | `Int n -> n
    | x -> bad_type "int" x

  let yojson_of_int n = `Int n

  let float_of_yojson = function
    | `Float f -> f
    | `Int n -> Float.of_int n
    | x -> bad_type "float" x

  let yojson_of_float f = `Float f

  let string_of_yojson = function
    | `String s -> s
    | x -> bad_type "string" x

  let yojson_of_string s = `String s

  let unit_of_yojson = function
    | `Null -> ()
    | x -> bad_type "null" x

  let yojson_of_unit () = `Null

  let list_of_yojson f = function
    | `List xs -> List.map f xs
    | x -> bad_type "array" x

  let yojson_of_list f xs = `List (List.map f xs)

  let option_of_yojson f = function
    | `String "None" -> None
    | `List [`String "Some"; x] -> Some (f x)
    | x -> bad_type "option" x

  let yojson_of_option f = function
    | None -> `String "None"
    | Some x -> `List [`String "Some"; f x]

  let nullable_of_yojson f = function
    | `Null -> None
    | x -> Some (f x)

  let yojson_of_nullable f = function
    | None -> `Null
    | Some x -> f x
end|};
  ]

(* ============ Type definition generation ============ *)

let gen_type_def tr ((loc, (name, params, an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let params_str = type_params_str params in
  match e with
  | Sum (_, variants, an) ->
      let is_poly =
        match get_ocaml_repr an with
        | Some "poly" -> true
        | _ -> false
      in
      let tick = if is_poly then "`" else "" in
      let flat = flatten_variants variants in
      let vtr =
        make_local_env
          (List.map (fun (_, orig_name, an, _) -> get_ocaml_name orig_name an) flat)
      in
      let gen_case (loc, orig_name, an, opt_e) =
        let cons_name = vtr (get_ocaml_name orig_name an) in
        match opt_e with
        | None ->
            with_inline_doc (sprintf "| %s%s" tick cons_name) loc an
        | Some e ->
            with_inline_doc
              (sprintf "| %s%s of %s" tick cons_name (type_expr_str tr e))
              loc an
      in
      if is_poly then
        [
          B.Line (sprintf "type %s%s = [" params_str ocaml_name);
          B.Block (concat_map gen_case flat);
          B.Line "]";
        ]
      else
        [
          B.Line (sprintf "type %s%s =" params_str ocaml_name);
          B.Block (concat_map gen_case flat);
        ]
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false
        ) fields
      in
      let ftr =
        make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
      in
      [
        B.Line (sprintf "type %s%s = {" params_str ocaml_name);
        B.Block
          (concat_map
             (fun (loc, (fname, _, an), e) ->
               with_inline_doc
                 (sprintf "%s: %s;" (ftr fname) (type_expr_str tr e))
                 loc an)
             fields);
        B.Line "}";
      ]
  | e ->
      [B.Line (sprintf "type %s%s = %s" params_str ocaml_name (type_expr_str tr e))]

(* ============ Creation function generation (records only) ============ *)

let get_implicit_default (e : type_expr) : string option =
  match e with
  | Name (_, (_, "unit", []), _) -> Some "()"
  | Name (_, (_, "bool", []), _) -> Some "false"
  | Name (_, (_, "int", []), _) -> Some "0"
  | Name (_, (_, "float", []), _) -> Some "0."
  | Name (_, (_, "string", []), _) -> Some {|""|}
  | List _ -> Some "[]"
  | Option _ -> Some "None"
  | Nullable _ -> Some "None"
  | _ -> None

let gen_make_fun tr ((loc, (name, params, an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  match e with
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false
        ) fields
      in
      let ftr =
        make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
      in
      let gen_param (_, (fname, kind, an), e) =
        let ofname = ftr fname in
        match kind with
        | Required -> sprintf "~%s" ofname
        | Optional -> sprintf "?%s" ofname
        | With_default ->
            let default =
              match get_ocaml_default an with
              | Some d -> d
              | None ->
                  match get_implicit_default e with
                  | Some d -> d
                  | None ->
                      A.error_at loc
                        (sprintf
                           "field '%s' needs a default value; \
                            use <ocaml default=\"...\"> annotation"
                           fname)
            in
            sprintf "?(%s = %s)" ofname default
      in
      let param_strs = List.map gen_param fields in
      let field_names =
        List.map (fun (_, (fname, _, _), _) -> ftr fname) fields
      in
      [
        B.Line
          (sprintf "let %s %s () : %s ="
             (make_name ocaml_name)
             (String.concat " " param_strs)
             (full_type_name ocaml_name params));
        B.Block
          [B.Line (sprintf "{ %s }" (String.concat "; " field_names))];
      ]
  | _ -> []

(* ============ Deserialization function generation ============ *)

let gen_of_yojson_field tr ftr type_name (loc, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  let ofname = ftr fname in
  let match_nodes =
    match kind with
    | Required ->
        [
          B.Line (sprintf "match List.assoc_opt \"%s\" fields with" json_name);
          B.Line (sprintf "| Some v -> %s v" (reader_expr tr e));
          B.Line
            (sprintf "| None -> Atdml_runtime.missing_field \"%s\" \"%s\""
               type_name json_name);
        ]
    | Optional ->
        let inner_e =
          match e with
          | Option (_, ie, _) -> ie
          | _ -> e
        in
        [
          B.Line (sprintf "match List.assoc_opt \"%s\" fields with" json_name);
          B.Line "| None | Some `Null -> None";
          B.Line (sprintf "| Some v -> Some (%s v)" (reader_expr tr inner_e));
        ]
    | With_default ->
        let default =
          match get_ocaml_default an with
          | Some d -> d
          | None ->
              match get_implicit_default e with
              | Some d -> d
              | None ->
                  A.error_at loc
                    (sprintf "field '%s' needs a default value" fname)
        in
        [
          B.Line (sprintf "match List.assoc_opt \"%s\" fields with" json_name);
          B.Line (sprintf "| None -> %s" default);
          B.Line (sprintf "| Some v -> %s v" (reader_expr tr e));
        ]
  in
  B.Inline
    [
      B.Line (sprintf "let %s =" ofname);
      B.Block match_nodes;
      B.Line "in";
    ]

let gen_of_yojson tr ((loc, (name, params, an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let param_strs = List.map (fun v -> "of_yojson_" ^ v) params in
  let extra_params =
    if param_strs = [] then ""
    else String.concat " " param_strs ^ " "
  in
  let return_type = full_type_name ocaml_name params in
  let body =
    match e with
    | Sum (_, variants, sum_an) ->
        let is_poly =
          match get_ocaml_repr sum_an with
          | Some "poly" -> true
          | _ -> false
        in
        let tick = if is_poly then "`" else "" in
        let flat = flatten_variants variants in
        let vtr =
          make_local_env
            (List.map (fun (_, orig_name, an, _) -> get_ocaml_name orig_name an) flat)
        in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let cons_name = vtr (get_ocaml_name orig_name an) in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| `String \"%s\" -> %s%s"
                   json_name tick cons_name)
          | Some e ->
              B.Line
                (sprintf "| `List [`String \"%s\"; v] -> %s%s (%s v)"
                   json_name tick cons_name (reader_expr tr e))
        in
        let (normalize, _) =
          adapter_exprs (Atd.Json.get_json_sum sum_an).json_sum_adapter
        in
        let pre = match normalize with
          | None -> []
          | Some f -> [B.Line (sprintf "let x = %s x in" f)]
        in
        B.Block
          (pre
           @ [B.Line "match x with"]
           @ List.map gen_case flat
           @ [B.Line (sprintf "| _ -> Atdml_runtime.bad_sum \"%s\" x" name)])
    | Record (_, fields, rec_an) ->
        let fields =
          List.filter_map (function
            | `Field x -> Some x
            | `Inherit _ -> assert false)
            fields
        in
        let ftr =
          make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
        in
        let field_names =
          List.map (fun (_, (fname, _, _), _) -> ftr fname) fields
        in
        let (normalize, _) =
          adapter_exprs (Atd.Json.get_json_record rec_an).json_record_adapter
        in
        let pre = match normalize with
          | None -> []
          | Some f -> [B.Line (sprintf "let x = %s x in" f)]
        in
        let match_block =
          B.Block
            (pre
             @ [ B.Line "match x with";
                 B.Line "| `Assoc fields ->";
                 B.Block
                   (List.map (gen_of_yojson_field tr ftr name) fields
                    @ [B.Line (sprintf "{ %s }" (String.concat "; " field_names))]);
                 B.Line (sprintf "| _ -> Atdml_runtime.bad_type \"%s\" x" name) ])
        in
        match_block
    | e ->
        B.Block [B.Line (sprintf "%s x" (reader_expr tr e))]
  in
  (* For parametric functions we use explicit universal quantification so that
     OCaml treats each function as polymorphic within the let rec...and block.
     Without this, a use site like 'result_of_yojson int_of_yojson' would fix
     the type of 'result_of_yojson' for the entire block. *)
  if params = [] then
    [
      B.Line (sprintf "let %s (x : Yojson.Safe.t) : %s ="
                (of_yojson_name ocaml_name) return_type);
      body;
    ]
  else
    let quant = String.concat " " (List.map (fun v -> "'" ^ v) params) in
    let param_types =
      String.concat " " (List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params)
    in
    let full_type = sprintf "%s Yojson.Safe.t -> %s" param_types return_type in
    [
      B.Line (sprintf "let %s : %s. %s =" (of_yojson_name ocaml_name) quant full_type);
      B.Block [B.Line (sprintf "fun %sx ->" extra_params); body];
    ]

(* ============ Serialization function generation ============ *)

let gen_yojson_of_field tr ftr (_, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  let ofname = ftr fname in
  match kind with
  | Required | With_default ->
      B.Line (sprintf "[(\"%s\", %s x.%s)];" json_name (writer_expr tr e) ofname)
  | Optional ->
      let inner_e =
        match e with
        | Option (_, ie, _) -> ie
        | _ -> e
      in
      B.Line
        (sprintf
           "(match x.%s with None -> [] | Some v -> [(\"%s\", %s v)]);"
           ofname json_name (writer_expr tr inner_e))

(* Wrap a writer body with a restore call when an adapter is present.
   Produces: let atdml_result_ = <body> in (restore) atdml_result_ *)
let apply_restore restore body =
  match restore with
  | None -> body
  | Some f ->
      B.Block [
        B.Line "let atdml_result_ =";
        body;
        B.Line (sprintf "in %s atdml_result_" f);
      ]

let gen_yojson_of tr ((loc, (name, params, an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let param_strs = List.map (fun v -> "yojson_of_" ^ v) params in
  let extra_params =
    if param_strs = [] then ""
    else String.concat " " param_strs ^ " "
  in
  let arg_type = full_type_name ocaml_name params in
  let body =
    match e with
    | Sum (_, variants, sum_an) ->
        let is_poly =
          match get_ocaml_repr sum_an with
          | Some "poly" -> true
          | _ -> false
        in
        let tick = if is_poly then "`" else "" in
        let flat = flatten_variants variants in
        let vtr =
          make_local_env
            (List.map (fun (_, orig_name, an, _) -> get_ocaml_name orig_name an) flat)
        in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let cons_name = vtr (get_ocaml_name orig_name an) in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| %s%s -> `String \"%s\""
                   tick cons_name json_name)
          | Some e ->
              B.Line
                (sprintf "| %s%s v -> `List [`String \"%s\"; %s v]"
                   tick cons_name json_name (writer_expr tr e))
        in
        let (_, restore) =
          adapter_exprs (Atd.Json.get_json_sum sum_an).json_sum_adapter
        in
        apply_restore restore
          (B.Block
            (B.Line "match x with"
             :: List.map gen_case flat))
    | Record (_, fields, rec_an) ->
        let fields =
          List.filter_map (function
            | `Field x -> Some x
            | `Inherit _ -> assert false)
            fields
        in
        let ftr =
          make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
        in
        let (_, restore) =
          adapter_exprs (Atd.Json.get_json_record rec_an).json_record_adapter
        in
        apply_restore restore
          (B.Block
            [
              B.Line "`Assoc (List.concat [";
              B.Block (List.map (gen_yojson_of_field tr ftr) fields);
              B.Line "])";
            ])
    | e ->
        B.Block [B.Line (sprintf "%s x" (writer_expr tr e))]
  in
  (* Same universal-quantification approach as gen_of_yojson *)
  if params = [] then
    [
      B.Line (sprintf "let %s (x : %s) : Yojson.Safe.t ="
                (yojson_of_name ocaml_name) arg_type);
      body;
    ]
  else
    let quant = String.concat " " (List.map (fun v -> "'" ^ v) params) in
    let param_types =
      String.concat " " (List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params)
    in
    let full_type = sprintf "%s %s -> Yojson.Safe.t" param_types arg_type in
    [
      B.Line (sprintf "let %s : %s. %s =" (yojson_of_name ocaml_name) quant full_type);
      B.Block [B.Line (sprintf "fun %sx ->" extra_params); body];
    ]

(* ============ Top-level I/O functions (all types, with converter args for parametric) ============ *)

let gen_io_funs tr ((_, (name, params, _), _) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let reader_params = List.map tvar_reader params in
  let writer_params = List.map tvar_writer params in
  let reader_args =
    if reader_params = [] then "" else String.concat " " reader_params ^ " "
  in
  let writer_args =
    if writer_params = [] then "" else String.concat " " writer_params ^ " "
  in
  let yojson_reader =
    if params = [] then of_yojson_name ocaml_name
    else sprintf "(%s %s)" (of_yojson_name ocaml_name) (String.concat " " reader_params)
  in
  let yojson_writer =
    if params = [] then yojson_of_name ocaml_name
    else sprintf "(%s %s)" (yojson_of_name ocaml_name) (String.concat " " writer_params)
  in
  [
    B.Line (sprintf "let %s %ss =" (of_json_name ocaml_name) reader_args);
    B.Block [B.Line (sprintf "%s (Yojson.Safe.from_string s)" yojson_reader)];
    B.Line "";
    B.Line (sprintf "let %s %sx =" (json_of_name ocaml_name) writer_args);
    B.Block [B.Line (sprintf "Yojson.Safe.to_string (%s x)" yojson_writer)];
  ]

(* ============ Submodule generation (.ml) ============ *)

let gen_submodule_ml tr ((_, (name, params, def_an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let params_str = type_params_str params in
  let type_decl =
    sprintf "type nonrec %st = %s%s" params_str (type_params_str params) ocaml_name
  in
  let attr_lines =
    match get_ocaml_attr def_an with
    | None -> []
    | Some attr -> [B.Line (sprintf "[@@%s]" attr)]
  in
  let make_binding =
    match e with
    | Record _ -> [B.Line (sprintf "let make = %s" (make_name ocaml_name))]
    | _ -> []
  in
  let bindings =
    make_binding
    @ [
        B.Line (sprintf "let of_yojson = %s" (of_yojson_name ocaml_name));
        B.Line (sprintf "let to_yojson = %s" (yojson_of_name ocaml_name));
        B.Line (sprintf "let of_json = %s" (of_json_name ocaml_name));
        B.Line (sprintf "let to_json = %s" (json_of_name ocaml_name));
      ]
  in
  [
    B.Line (sprintf "module %s = struct" (module_name ocaml_name));
    B.Block (B.Line type_decl :: attr_lines @ bindings);
    B.Line "end";
  ]

(* ============ MLI: type signatures ============ *)

let gen_of_yojson_sig tr ((_, (name, params, _), _) : A.type_def) : B.t =
  let ocaml_name = tr name in
  match params with
  | [] ->
      [B.Line (sprintf "val %s : Yojson.Safe.t -> %s"
                 (of_yojson_name ocaml_name) ocaml_name)]
  | _ ->
      let param_sigs = List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params in
      let return_type = full_type_name ocaml_name params in
      [
        B.Line (sprintf "val %s :" (of_yojson_name ocaml_name));
        B.Block
          (List.map (fun s -> B.Line s) param_sigs
           @ [B.Line "Yojson.Safe.t ->"; B.Line return_type]);
      ]

let gen_yojson_of_sig tr ((_, (name, params, _), _) : A.type_def) : B.t =
  let ocaml_name = tr name in
  match params with
  | [] ->
      [B.Line (sprintf "val %s : %s -> Yojson.Safe.t"
                 (yojson_of_name ocaml_name) ocaml_name)]
  | _ ->
      let param_sigs = List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params in
      let arg_type = full_type_name ocaml_name params in
      [
        B.Line (sprintf "val %s :" (yojson_of_name ocaml_name));
        B.Block
          (List.map (fun s -> B.Line s) param_sigs
           @ [B.Line (sprintf "%s ->" arg_type); B.Line "Yojson.Safe.t"]);
      ]

let gen_make_sig tr ((loc, (name, params, _), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  match e with
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false)
          fields
      in
      let ftr =
        make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
      in
      let gen_param_sig (loc, (fname, kind, an), e) =
        let ofname = ftr fname in
        match kind with
        | Required -> sprintf "%s:%s ->" ofname (type_expr_str tr e)
        | Optional ->
            let inner =
              match e with
              | Option (_, ie, _) -> ie
              | _ -> e
            in
            sprintf "?%s:%s ->" ofname (type_expr_str tr inner)
        | With_default -> sprintf "?%s:%s ->" ofname (type_expr_str tr e)
      in
      let param_sigs = List.map gen_param_sig fields in
      let return_type = full_type_name ocaml_name params in
      [
        B.Line
          (sprintf "val %s : %sunit -> %s"
             (make_name ocaml_name)
             (String.concat " " param_sigs ^ " ")
             return_type);
      ]
  | _ -> []

let gen_io_sigs tr ((_, (name, params, _), _) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let return_type = full_type_name ocaml_name params in
  match params with
  | [] ->
      [
        B.Line (sprintf "val %s : string -> %s" (of_json_name ocaml_name) ocaml_name);
        B.Line (sprintf "val %s : %s -> string" (json_of_name ocaml_name) ocaml_name);
      ]
  | _ ->
      let reader_sigs =
        List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params
      in
      let writer_sigs =
        List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params
      in
      [
        B.Line (sprintf "val %s :" (of_json_name ocaml_name));
        B.Block
          (List.map (fun s -> B.Line s) reader_sigs
           @ [B.Line "string ->"; B.Line return_type]);
        B.Line (sprintf "val %s :" (json_of_name ocaml_name));
        B.Block
          (List.map (fun s -> B.Line s) writer_sigs
           @ [B.Line (sprintf "%s ->" return_type); B.Line "string"]);
      ]

(* ============ Submodule signature generation (.mli) ============ *)

let gen_submodule_mli tr ((loc, (name, params, def_an), e) : A.type_def) : B.t =
  let ocaml_name = tr name in
  let params_str = type_params_str params in
  let t_type = type_params_str params ^ "t" in
  let type_decl =
    sprintf "type nonrec %st = %s%s" params_str (type_params_str params) ocaml_name
  in
  let attr_lines =
    match get_ocaml_attr def_an with
    | None -> []
    | Some attr -> [B.Line (sprintf "[@@%s]" attr)]
  in
  let make_sig =
    match e with
    | Record (_, fields, _) ->
        let fields =
          List.filter_map (function
            | `Field x -> Some x
            | `Inherit _ -> assert false)
            fields
        in
        let ftr =
          make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
        in
        let gen_param_sig (loc, (fname, kind, an), e) =
          let ofname = ftr fname in
          match kind with
          | Required -> sprintf "%s:%s ->" ofname (type_expr_str tr e)
          | Optional ->
              let inner =
                match e with
                | Option (_, ie, _) -> ie
                | _ -> e
              in
              sprintf "?%s:%s ->" ofname (type_expr_str tr inner)
          | With_default -> sprintf "?%s:%s ->" ofname (type_expr_str tr e)
        in
        let param_sigs = List.map gen_param_sig fields in
        [
          B.Line
            (sprintf "val make : %sunit -> %s"
               (String.concat " " param_sigs ^ " ")
               t_type);
        ]
    | _ -> []
  in
  let of_yojson_sig =
    match params with
    | [] -> [B.Line (sprintf "val of_yojson : Yojson.Safe.t -> %s" t_type)]
    | _ ->
        let param_sigs =
          List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params
        in
        [
          B.Line "val of_yojson :";
          B.Block
            (List.map (fun s -> B.Line s) param_sigs
             @ [B.Line "Yojson.Safe.t ->"; B.Line t_type]);
        ]
  in
  let to_yojson_sig =
    match params with
    | [] -> [B.Line (sprintf "val to_yojson : %s -> Yojson.Safe.t" t_type)]
    | _ ->
        let param_sigs =
          List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params
        in
        [
          B.Line "val to_yojson :";
          B.Block
            (List.map (fun s -> B.Line s) param_sigs
             @ [B.Line (sprintf "%s ->" t_type); B.Line "Yojson.Safe.t"]);
        ]
  in
  let of_json_sig =
    match params with
    | [] -> [B.Line (sprintf "val of_json : string -> %s" t_type)]
    | _ ->
        let param_sigs =
          List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params
        in
        [
          B.Line "val of_json :";
          B.Block
            (List.map (fun s -> B.Line s) param_sigs
             @ [B.Line "string ->"; B.Line t_type]);
        ]
  in
  let to_json_sig =
    match params with
    | [] -> [B.Line (sprintf "val to_json : %s -> string" t_type)]
    | _ ->
        let param_sigs =
          List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params
        in
        [
          B.Line "val to_json :";
          B.Block
            (List.map (fun s -> B.Line s) param_sigs
             @ [B.Line (sprintf "%s ->" t_type); B.Line "string"]);
        ]
  in
  let body =
    [B.Line type_decl]
    @ attr_lines
    @ make_sig
    @ of_yojson_sig
    @ to_yojson_sig
    @ of_json_sig
    @ to_json_sig
  in
  [
    B.Line (sprintf "module %s : sig" (module_name ocaml_name));
    B.Block body;
    B.Line "end";
  ]

(* ============ Group emission helpers (based on Atd.Util.tsort output) ============ *)

(* Emit type definitions for one tsort group, connecting them with 'and'
   when there are multiple defs (mutual recursion).
   Appends [@@attr] when an <ocaml attr="..."> annotation is present.
   Prepends a (** ... *) ocamldoc comment when a <doc text="..."> annotation
   is present. *)
let emit_type_group tr (defs : A.type_def list) : B.t =
  let attr_line (_, (_, _, def_an), _) =
    match get_ocaml_attr def_an with
    | None -> []
    | Some attr -> [B.Line (sprintf "[@@%s]" attr)]
  in
  let doc_lines ((loc, (_, _, def_an), _) : A.type_def) : B.t =
    doc_comment_prepend loc def_an
  in
  match defs with
  | [] -> []
  | [def] -> doc_lines def @ gen_type_def tr def @ attr_line def @ [B.Line ""]
  | first :: rest ->
      let first_lines = doc_lines first @ gen_type_def tr first @ attr_line first in
      let rest_lines =
        concat_map (fun def ->
          let lines =
            match gen_type_def tr def with
            | B.Line s :: tl when String.length s >= 4
              && String.sub s 0 4 = "type" ->
                B.Line ("and" ^ String.sub s 4 (String.length s - 4)) :: tl
            | lines -> lines
          in
          B.Line "" :: doc_lines def @ lines @ attr_line def
        ) rest
      in
      first_lines @ rest_lines @ [B.Line ""]

(* Replace the leading "let" keyword in a B.t block. *)
let replace_let keyword = function
  | B.Line s :: rest when String.length s >= 3 && String.sub s 0 3 = "let" ->
      B.Line (keyword ^ String.sub s 3 (String.length s - 3)) :: rest
  | lines -> lines

(* Emit function definitions for one tsort group.
   Uses 'let rec' (and 'and' for subsequent) when is_recursive,
   plain 'let' otherwise. *)
let emit_fun_group ~is_recursive gen_fun (defs : A.type_def list) : B.t =
  let funs = List.map gen_fun defs in
  let first_kw = if is_recursive then "let rec" else "let" in
  match funs with
  | [] -> []
  | [f] -> replace_let first_kw f @ [B.Line ""]
  | first :: rest ->
      replace_let first_kw first
      @ concat_map (fun f -> B.Line "" :: replace_let "and" f) rest
      @ [B.Line ""]

(* ============ Assemble the .ml file ============ *)

let make_ml ~tr ~atd_filename ~module_doc (items : A.module_body) : B.t =
  let header =
    [
      B.Line (sprintf "(* Auto-generated from \"%s\" by atdml. *)"
                atd_filename);
      B.Line "[@@@ocaml.warning \"-27-32-33-35-39\"]";
      B.Line "";
    ]
    @ module_doc
    @ (if module_doc = [] then [] else [B.Line ""])
  in
  let gen_group (is_recursive, group_items) =
    let defs = List.map (fun (Type x) -> x) group_items in
    let types = emit_type_group tr defs in
    let makes =
      concat_map (fun def ->
        let lines = gen_make_fun tr def in
        if lines = [] then [] else lines @ [B.Line ""]
      ) defs
    in
    let of_yojsons = emit_fun_group ~is_recursive (gen_of_yojson tr) defs in
    let yojson_ofs = emit_fun_group ~is_recursive (gen_yojson_of tr) defs in
    let ios =
      concat_map (fun def -> gen_io_funs tr def @ [B.Line ""]) defs
    in
    let submods =
      concat_map (fun def -> gen_submodule_ml tr def @ [B.Line ""]) defs
    in
    types @ makes @ of_yojsons @ yojson_ofs @ ios @ submods
  in
  header
  @ runtime_module
  @ [B.Line ""]
  @ concat_map gen_group (Atd.Util.tsort items)

(* ============ Assemble the .mli file ============ *)

let make_mli ~tr ~atd_filename ~module_doc (items : A.module_body) : B.t =
  let header =
    [
      B.Line (sprintf "(* Auto-generated from \"%s\" by atdml. *)"
                atd_filename);
      B.Line "";
    ]
    @ module_doc
    @ (if module_doc = [] then [] else [B.Line ""])
  in
  let gen_group (_, group_items) =
    let defs = List.map (fun (Type x) -> x) group_items in
    let types = emit_type_group tr defs in
    let sigs =
      concat_map (fun def ->
        gen_make_sig tr def
        @ gen_of_yojson_sig tr def
        @ gen_yojson_of_sig tr def
        @ gen_io_sigs tr def
        @ [B.Line ""]
      ) defs
    in
    let submod_sigs =
      concat_map (fun def -> gen_submodule_mli tr def @ [B.Line ""]) defs
    in
    types @ sigs @ submod_sigs
  in
  header @ concat_map gen_group (Atd.Util.tsort items)

(* ============ Entry points ============ *)

(*
   Generate a self-contained OCaml snippet from ATD source, suitable for
   copy-pasting into utop or ocaml:

     module type Types = sig
       [mli content]
     end

     module Types : Types = struct
       [ml content]
     end
*)
let run_stdin () =
  let full_module, _ =
    Atd.Util.read_channel
      ~annot_schema
      ~expand:false
      ~keep_builtins:false
      ~inherit_fields:true
      ~inherit_variants:true
      stdin
  in
  let (head_loc, head_an), atd_module = full_module in
  let module_doc = doc_comment_prepend head_loc head_an in
  let defs = List.map (fun (Type x) -> x) atd_module in
  let tr = init_env defs in
  let mli = make_mli ~tr ~atd_filename:"<stdin>" ~module_doc atd_module in
  let ml = make_ml ~tr ~atd_filename:"<stdin>" ~module_doc atd_module in
  B.to_stdout ~indent:2
    [
      B.Line "module type Types = sig";
      B.Block mli;
      B.Line "end";
      B.Line "";
      B.Line "module Types : Types = struct";
      B.Block ml;
      B.Line "end";
    ]

let run_file src_path =
  let src_name = Filename.basename src_path in
  let base_name =
    if Filename.check_suffix src_name ".atd" then
      Filename.chop_suffix src_name ".atd"
    else
      src_name
  in
  let ml_path = String.lowercase_ascii base_name ^ ".ml" in
  let mli_path = String.lowercase_ascii base_name ^ ".mli" in
  let full_module, _original_types =
    Atd.Util.load_file
      ~annot_schema
      ~expand:false
      ~keep_builtins:false
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let (head_loc, head_an), atd_module = full_module in
  let module_doc = doc_comment_prepend head_loc head_an in
  let defs = List.map (fun (Type x) -> x) atd_module in
  let tr = init_env defs in
  let ml_contents = make_ml ~tr ~atd_filename:src_name ~module_doc atd_module in
  let mli_contents = make_mli ~tr ~atd_filename:src_name ~module_doc atd_module in
  B.to_file ~indent:2 ml_path ml_contents;
  B.to_file ~indent:2 mli_path mli_contents
