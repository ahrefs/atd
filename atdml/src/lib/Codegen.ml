(*
   OCaml code generation for JSON support using the Yojson AST.

   For each ATD type 'foo', the generated code contains:
   - A type definition 'type foo = ...'
     For unparameterized aliases of primitive types (unit, bool, int, float,
     string), the .mli uses 'type foo = private <prim>' so that the compiler
     refers to the alias name rather than the underlying type in error messages.
   - For record types: 'let create_foo ...' with labeled arguments
   - For primitive aliases: 'let create_foo (x : <prim>) : foo = x'
     Coercion back to the primitive is done with the ':>' operator.
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
    Type_def,  "private"; (* <ocaml private> on a type def: emit 'private' in .mli *)
    Type_def,  "public";  (* <ocaml public> on a type def: suppress default 'private' *)
    Type_def,  "module";  (* <ocaml module="M"> on a type def: not supported, warns *)
    Type_def,  "t";       (* <ocaml t="..."> on a type def: not supported, warns *)
    Type_expr, "repr";         (* <ocaml repr="poly"> on a sum type *)
    Type_expr, "field_prefix"; (* <ocaml field_prefix="t_"> on a record type *)
    Type_expr, "module";       (* <ocaml module="M"> on a wrap: use M.t/M.wrap/M.unwrap *)
    Type_expr, "t";            (* <ocaml t="..."> on a wrap: explicit OCaml type *)
    Type_expr, "wrap";         (* <ocaml wrap="..."> on a wrap: deserialize function *)
    Type_expr, "unwrap";       (* <ocaml unwrap="..."> on a wrap: serialize function *)
    Import, "name";            (* <ocaml name="..."> on an import: override OCaml alias *)
    Imported_type, "name";    (* <ocaml name="..."> on an imported type: override type name *)
    Variant, "name";      (* <ocaml name="..."> on a variant constructor *)
    Field, "default";     (* <ocaml default="..."> on a with-default field *)
    Field, "name";        (* <ocaml name="..."> on a field: not supported, warns *)
  ]
}

let annot_schema : Atd.Annot.schema =
  annot_schema_ocaml :: (Atd.Json.annot_schema_json @ Atd.Doc.annot_schema)

(* ============ Errors ============ *)

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdml: " ^ msg)

let warn_not_supported loc annot_str =
  eprintf "Warning: %s: %s is not supported by atdml; annotation ignored.\n%!"
    (A.string_of_loc loc) annot_str

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

(* The OCaml module path for an import, built from all path components.
   E.g. import foo → Foo; import long.module.path → Long.Module.Path *)
let ocaml_module_of_import (x : A.import) =
  String.concat "." (List.map String.capitalize_ascii x.path)

(* The OCaml alias to use in the .ml file for an import.
   Priority: <ocaml name> on path annot > alias name > last path component *)
let ocaml_name_of_import (x : A.import) =
  match Atd.Annot.get_opt_field
          ~parse:(fun s -> Some s) ~sections:["ocaml"] ~field:"name" x.annot
  with
  | Some name -> name
  | None -> String.capitalize_ascii x.name

type env = {
  tr: string -> string;      (* translate local ATD type name → OCaml identifier *)
  imports: Atd.Imports.t;    (* resolved import table *)
  (* How to refer to an imported module in generated code.
     Different for .ml (use alias) vs .mli (use last path component). *)
  ocaml_mod_of_import: A.import -> string;
}

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
let init_env (imports : A.import list) (defs : A.type_def list) : env =
  let registry =
    Atd.Unique_name.init
      ~reserved_identifiers:(ocaml_keywords @ naming_conflicts)
      ~reserved_prefixes:[]
      ~safe_prefix:"x_"
  in
  (* Register all type names upfront so conflict resolution is deterministic
     regardless of where in the file each name is first referenced. *)
  List.iter (fun (def : A.type_def) ->
    ignore (Atd.Unique_name.translate registry (Atd.Type_name.basename def.name))
  ) defs;
  let imports = Atd.Imports.load imports in
  { tr = Atd.Unique_name.translate registry;
    imports;
    ocaml_mod_of_import = ocaml_module_of_import }

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

(*
   Build two name translators for a prefixed record:
   - label_tr: ATD field name → OCaml-safe label name (no prefix applied).
     E.g. "if" → "if_".
   - pftr: ATD field name → OCaml-safe record field name (prefix concatenated
     with the raw ATD name *before* checking for keyword conflicts).
     E.g. with prefix "mod": "ule" → "module_", "if" → "modif".
   When prefix = "" both translators are identical.
*)
let make_prefixed_trs prefix field_names =
  let label_tr = make_local_env field_names in
  let pftr =
    if prefix = "" then label_tr
    else
      let pnames = List.map (fun n -> prefix ^ n) field_names in
      let field_tr = make_local_env pnames in
      fun fname -> field_tr (prefix ^ fname)
  in
  (label_tr, pftr)

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

(* Derive the normalize_jsonlike expression from a module-based adapter.
   Module-based adapters have normalize = "M.normalize"; we produce
   "M.normalize_jsonlike". Inline adapters (adapter.to_ocaml/from_ocaml)
   wrap the expression in parens and are not supported for jsonlike. *)
let normalize_jsonlike_of_adapter (adapter : Atd.Json.json_adapter) =
  match adapter.ocaml_adapter with
  | None -> None
  | Some a ->
      let suffix = ".normalize" in
      let n = a.normalize in
      let slen = String.length suffix and nlen = String.length n in
      if nlen > slen && String.sub n (nlen - slen) slen = suffix then
        Some (String.sub n 0 (nlen - slen) ^ ".normalize_jsonlike")
      else
        None

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

(* Get <ocaml field_prefix="..."> from a record type expression's annotation.
   Returns the prefix string, or "" if the annotation is absent. *)
let get_field_prefix rec_an =
  match Atd.Annot.get_opt_field
          ~parse:(fun s -> Some s)
          ~sections:["ocaml"] ~field:"field_prefix"
          rec_an
  with
  | None -> ""
  | Some prefix -> prefix

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
let of_jsonlike_name name = name ^ "_of_jsonlike"
let of_json_name name = name ^ "_of_json"
let json_of_name name = "json_of_" ^ name
let create_name name = "create_" ^ name
let module_name name = String.capitalize_ascii name

(* Emit 'module Alias = OcamlMod' for imports where the local alias name
   (capitalized) differs from the full OCaml module path.
   These go at the top of the .ml file only. *)
let module_alias_decls (imports : A.import list) : B.t =
  List.filter_map (fun (x : A.import) ->
    let ocaml_alias = ocaml_name_of_import x in
    let ocaml_mod = ocaml_module_of_import x in
    if ocaml_alias <> ocaml_mod then
      Some (B.Line (sprintf "module %s = %s" ocaml_alias ocaml_mod))
    else
      None
  ) imports

(* Function parameter name for reading/writing a type variable *)
let tvar_reader v = "of_yojson_" ^ v
let tvar_writer v = "yojson_of_" ^ v

(* ============ Code generation mode ============ *)
(*
   Controls whether reader functions consume Yojson.Safe.t or Atd_jsonlike.AST.t.
*)
type mode = {
  tree_type: string;
    (** OCaml type of the input tree node, e.g. "Yojson.Safe.t" *)
  runtime: string;
    (** Runtime submodule path, e.g. "Atdml_runtime.Yojson" *)
  suffix: string;
    (** Suffix for reader function names, e.g. "yojson" or "jsonlike" *)
  of_name: string -> string;
    (** Build the reader function name for a user type, e.g. "foo_of_yojson" *)
  mode_tvar_reader: string -> string;
    (** Type-variable reader param name, e.g. "of_yojson_a" *)
  normalize_of_adapter: Atd.Json.json_adapter -> string option;
    (** Extract the normalize expression for this mode from an adapter annotation.
        Returns [None] if the adapter is not applicable for this mode. *)
  tuple_arr_pat: int -> string;
    (** Pattern string matching an array of exactly n elements *)
  sum_unit_pat: string -> string;
    (** Pattern for a unit-like sum constructor given its JSON name *)
  sum_tagged_pat: string -> string;
    (** Pattern for a tagged sum constructor (array repr) given its JSON name *)
  sum_object_tagged_pat: string -> string;
    (** Pattern for a tagged sum constructor (object repr) given its JSON name *)
  record_match_pat: string;
    (** Pattern for matching a record node *)
  record_assoc_lines: B.t;
    (** B.t block that binds [assoc_] for field lookup *)
  optional_null_pat: string;
    (** Pattern(s) for a null/absent optional field *)
  record_node_binding: B.t;
    (** Lines emitted at the start of the record match arm to save the input
        node under a stable name before field bindings may shadow [x] *)
  missing_req_expr: string -> string -> string;
    (** missing_req_expr type_name field_name → full error expression string *)
  abstract_reader: string;
    (** Reader expression for the ATD 'abstract' type *)
}

let yojson_mode = {
  tree_type = "Yojson.Safe.t";
  runtime = "Atdml_runtime.Yojson";
  suffix = "yojson";
  of_name = of_yojson_name;
  mode_tvar_reader = (fun v -> "of_yojson_" ^ v);
  normalize_of_adapter = (fun adapter ->
    Option.map (fun a -> a.Atd.Json.normalize) adapter.Atd.Json.ocaml_adapter);
  tuple_arr_pat = (fun n ->
    sprintf "`List lst when List.length lst = %d" n);
  sum_unit_pat = (fun jn -> sprintf "`String \"%s\"" jn);
  sum_tagged_pat = (fun jn -> sprintf "`List [`String \"%s\"; v]" jn);
  sum_object_tagged_pat = (fun jn -> sprintf "`Assoc [(\"%s\", v)]" jn);
  record_match_pat = "`Assoc fields";
  record_assoc_lines = [
    B.Line "(* Duplicate JSON keys: behavior is unspecified (RFC 8259 §4 says keys SHOULD";
    B.Line "   be unique). Below the threshold, List.assoc_opt returns the first binding;";
    B.Line "   above it, the hashtable returns the last. *)";
    B.Line "let assoc_ =";
    B.Block
      [ B.Line "if Atdml_runtime.list_length_gt 5 fields then";
        B.Block
          [ B.Line "let tbl = Hashtbl.create 16 in";
            B.Line "List.iter (fun (k, v) -> Hashtbl.add tbl k v) fields;";
            B.Line "(fun key -> Hashtbl.find_opt tbl key)" ];
        B.Line "else (fun key -> List.assoc_opt key fields)" ];
    B.Line "in";
  ];
  optional_null_pat = "None | Some `Null";
  record_node_binding = [];
  missing_req_expr = (fun tn fn ->
    sprintf "Atdml_runtime.Yojson.missing_field \"%s\" \"%s\"" tn fn);
  abstract_reader = "(fun x -> x)";
}

let jsonlike_mode = {
  tree_type = "Atd_jsonlike.AST.t";
  runtime = "Atdml_runtime.Jsonlike";
  suffix = "jsonlike";
  of_name = of_jsonlike_name;
  mode_tvar_reader = (fun v -> "of_jsonlike_" ^ v);
  normalize_of_adapter = normalize_jsonlike_of_adapter;
  tuple_arr_pat = (fun n ->
    sprintf "Atd_jsonlike.AST.Array (_, lst) when List.length lst = %d" n);
  sum_unit_pat = (fun jn ->
    sprintf "Atd_jsonlike.AST.String (_, \"%s\")" jn);
  sum_tagged_pat = (fun jn ->
    sprintf "Atd_jsonlike.AST.Array (_, [Atd_jsonlike.AST.String (_, \"%s\"); v])" jn);
  sum_object_tagged_pat = (fun jn ->
    sprintf "Atd_jsonlike.AST.Object (_, [(_, \"%s\", v)])" jn);
  record_match_pat = "Atd_jsonlike.AST.Object (_, fields)";
  record_assoc_lines = [
    B.Line "let assoc_ =";
    B.Block
      [ B.Line "if Atdml_runtime.list_length_gt 5 fields then";
        B.Block
          [ B.Line "let tbl = Hashtbl.create 16 in";
            B.Line "List.iter (fun (_, k, v) -> Hashtbl.add tbl k v) fields;";
            B.Line "(fun key -> Hashtbl.find_opt tbl key)" ];
        B.Line "else";
        B.Block
          [ B.Line "(fun key ->";
            B.Block
              [ B.Line "match List.find_opt (fun (_, k, _) -> k = key) fields with";
                B.Line "| None -> None | Some (_, _, v) -> Some v)" ] ] ];
    B.Line "in";
  ];
  optional_null_pat = "None | Some (Atd_jsonlike.AST.Null _)";
  record_node_binding = [B.Line "let atdml_node_ = x in"];
  missing_req_expr = (fun tn fn ->
    sprintf "Atdml_runtime.Jsonlike.missing_field atdml_node_ \"%s\" \"%s\"" tn fn);
  abstract_reader =
    "(fun _ -> failwith \"abstract type is not supported in jsonlike mode\")";
}

(* ============ Builtin OCaml type names ============ *)

let is_atd_builtin = function
  | "unit" | "bool" | "int" | "float" | "string" | "abstract" -> true
  | _ -> false

(* Returns the ATD primitive name ('unit', 'bool', 'int', 'float', or 'string')
   if the type definition is an unparameterized alias of exactly that primitive.
   Used to decide whether to emit 'private' in the .mli and generate create/to_*
   helper functions. 'abstract' is intentionally excluded. *)
let primitive_alias_of_def ({A.param; value=e; _} : A.type_def) =
  match param, e with
  | [], Name (_, (_, TN [name], []), _)
    when List.mem name ["unit"; "bool"; "int"; "float"; "string"] ->
      Some name
  | _ -> None

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
let rec type_expr_str env (e : type_expr) : string =
  match e with
  | Sum _ | Record _ -> failwith "inline types are not supported"
  | Tuple (_, cells, _) ->
      let parts = List.map (fun (_, e, _) -> type_expr_str env e) cells in
      "(" ^ String.concat " * " parts ^ ")"
  | List (_, e, _) -> type_expr_str env e ^ " list"
  | Option (_, e, _) -> type_expr_str env e ^ " option"
  | Nullable (_, e, _) -> type_expr_str env e ^ " option"
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let (wrap_t, _, _) = get_ocaml_wrap an in
      (match wrap_t with
      | Some t -> t
      | None -> type_expr_str env inner_e)
  | Name (_, (_, TN [name], []), _) ->
      if is_atd_builtin name then builtin_ocaml_name name else env.tr name
  | Name (_, (_, TN [name], params), _) ->
      let pstrs = List.map (type_expr_str env) params in
      "(" ^ String.concat ", " pstrs ^ ") " ^ env.tr name
  | Name (loc, (_, name, params), _) ->
      (* qualified type name: resolve via imports *)
      let (import_opt, base_name) = Atd.Imports.resolve env.imports loc name in
      (match import_opt with
       | None -> assert false (* already handled by TN [name] cases above *)
       | Some (import, _) ->
           let ocaml_mod = env.ocaml_mod_of_import import in
           match params with
           | [] -> sprintf "%s.%s" ocaml_mod base_name
           | _ ->
               let pstrs = List.map (type_expr_str env) params in
               sprintf "(%s) %s.%s" (String.concat ", " pstrs) ocaml_mod base_name)
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

let rec reader_expr env mode (e : type_expr) : string =
  let rt = mode.runtime in
  let sfx = mode.suffix in
  match e with
  | Name (_, (_, TN ["unit"], []), _) -> rt ^ ".unit_of_" ^ sfx
  | Name (_, (_, TN ["bool"], []), _) -> rt ^ ".bool_of_" ^ sfx
  | Name (_, (_, TN ["int"], []), _) -> rt ^ ".int_of_" ^ sfx
  | Name (_, (_, TN ["float"], []), _) -> rt ^ ".float_of_" ^ sfx
  | Name (_, (_, TN ["string"], []), _) -> rt ^ ".string_of_" ^ sfx
  | Name (_, (_, TN ["abstract"], []), _) -> mode.abstract_reader
  | Name (_, (_, TN [name], []), _) -> mode.of_name (env.tr name)
  | Name (_, (_, TN [name], params), _) ->
      let readers = List.map (reader_expr env mode) params in
      sprintf "(%s %s)" (mode.of_name (env.tr name)) (String.concat " " readers)
  | Name (loc, (_, name, params), _) ->
      let (import_opt, base_name) = Atd.Imports.resolve env.imports loc name in
      (match import_opt with
       | None -> assert false
       | Some (import, _) ->
           let ocaml_mod = env.ocaml_mod_of_import import in
           let fn = sprintf "%s.%s" ocaml_mod (mode.of_name base_name) in
           (match params with
            | [] -> fn
            | _ ->
                let readers = List.map (reader_expr env mode) params in
                sprintf "(%s %s)" fn (String.concat " " readers)))
  | List (_, Tuple (_, [(_, Name (_, (_, TN ["string"], []), _), _); (_, val_e, _)], _), an)
    when Atd.Json.get_json_list an = Atd.Json.Object ->
      sprintf "(%s.assoc_of_%s %s)" rt sfx (reader_expr env mode val_e)
  | List (_, e, _) ->
      sprintf "(%s.list_of_%s %s)" rt sfx (reader_expr env mode e)
  | Option (_, e, _) ->
      sprintf "(%s.option_of_%s %s)" rt sfx (reader_expr env mode e)
  | Nullable (_, e, _) ->
      sprintf "(%s.nullable_of_%s %s)" rt sfx (reader_expr env mode e)
  | Tuple (_, cells, _) ->
      let n = List.length cells in
      let reads =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s (List.nth lst %d)" (reader_expr env mode e) i
        ) cells
      in
      sprintf
        "(fun x -> match x with \
         | %s -> (%s) \
         | _ -> %s.bad_type \"tuple\" x)"
        (mode.tuple_arr_pat n)
        (String.concat ", " reads)
        rt
  | Tvar (_, v) -> mode.mode_tvar_reader v
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let inner = reader_expr env mode inner_e in
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

let rec writer_expr env (e : type_expr) : string =
  match e with
  | Name (_, (_, TN ["unit"], []), _) -> "Atdml_runtime.Yojson.yojson_of_unit"
  | Name (_, (_, TN ["bool"], []), _) -> "Atdml_runtime.Yojson.yojson_of_bool"
  | Name (_, (_, TN ["int"], []), _) -> "Atdml_runtime.Yojson.yojson_of_int"
  | Name (_, (_, TN ["float"], []), _) -> "Atdml_runtime.Yojson.yojson_of_float"
  | Name (_, (_, TN ["string"], []), _) -> "Atdml_runtime.Yojson.yojson_of_string"
  | Name (_, (_, TN ["abstract"], []), _) -> "(fun x -> x)"
  | Name (_, (_, TN [name], []), _) -> yojson_of_name (env.tr name)
  | Name (_, (_, TN [name], params), _) ->
      let writers = List.map (writer_expr env) params in
      sprintf "(%s %s)" (yojson_of_name (env.tr name)) (String.concat " " writers)
  | Name (loc, (_, name, params), _) ->
      let (import_opt, base_name) = Atd.Imports.resolve env.imports loc name in
      (match import_opt with
       | None -> assert false
       | Some (import, _) ->
           let ocaml_mod = env.ocaml_mod_of_import import in
           let fn = sprintf "%s.%s" ocaml_mod (yojson_of_name base_name) in
           (match params with
            | [] -> fn
            | _ ->
                let writers = List.map (writer_expr env) params in
                sprintf "(%s %s)" fn (String.concat " " writers)))
  | List (_, Tuple (_, [(_, Name (_, (_, TN ["string"], []), _), _); (_, val_e, _)], _), an)
    when Atd.Json.get_json_list an = Atd.Json.Object ->
      sprintf "(Atdml_runtime.Yojson.yojson_of_assoc %s)" (writer_expr env val_e)
  | List (_, e, _) ->
      sprintf "(Atdml_runtime.Yojson.yojson_of_list %s)" (writer_expr env e)
  | Option (_, e, _) ->
      sprintf "(Atdml_runtime.Yojson.yojson_of_option %s)" (writer_expr env e)
  | Nullable (_, e, _) ->
      sprintf "(Atdml_runtime.Yojson.yojson_of_nullable %s)" (writer_expr env e)
  | Tuple (_, cells, _) ->
      let vars = List.mapi (fun i _ -> sprintf "x%d" i) cells in
      let writes =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s %s" (writer_expr env e) (List.nth vars i)
        ) cells
      in
      sprintf "(fun (%s) -> `List [%s])"
        (String.concat ", " vars)
        (String.concat "; " writes)
  | Tvar (_, v) -> tvar_writer v
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (_, inner_e, an) ->
      let inner = writer_expr env inner_e in
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

let runtime_module ~yojson ~jsonlike : B.t =
  let preamble = {|(* Inlined runtime — no external dependency needed. *)
module Atdml_runtime = struct
  (* Returns true iff the list has strictly more than [n] elements,
     without traversing past element n+1. *)
  let rec list_length_gt n = function
    | _ :: rest -> if n = 0 then true else list_length_gt (n - 1) rest
    | [] -> false|} in
  let yojson_ext = {|

  module Yojson = struct
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

    let assoc_of_yojson f = function
      | `Assoc pairs -> List.map (fun (k, v) -> (k, f v)) pairs
      | x -> bad_type "object" x

    let yojson_of_assoc f xs =
      `Assoc (List.map (fun (k, v) -> (k, f v)) xs)
  end|} in
  let jsonlike_ext = {|

  module Jsonlike = struct
    let bad_type expected_type x =
      Printf.ksprintf failwith "%sexpected %s"
        (Atd_jsonlike.AST.loc_msg x) expected_type

    let bad_sum type_name x =
      Printf.ksprintf failwith "%sinvalid variant for type '%s'"
        (Atd_jsonlike.AST.loc_msg x) type_name

    let missing_field node type_name field_name =
      Printf.ksprintf failwith "%smissing field '%s' in object of type '%s'"
        (Atd_jsonlike.AST.loc_msg node) field_name type_name

    let bool_of_jsonlike = function
      | Atd_jsonlike.AST.Bool (_, b) -> b
      | x -> bad_type "bool" x

    let int_of_jsonlike = function
      | Atd_jsonlike.AST.Number (_, n) as node ->
          (match n.Atd_jsonlike.Number.int with
          | Some i -> i
          | None -> bad_type "integer" node)
      | x -> bad_type "int" x

    let float_of_jsonlike = function
      | Atd_jsonlike.AST.Number (_, n) as node ->
          (match n.Atd_jsonlike.Number.float with
          | Some f -> f
          | None -> bad_type "float" node)
      | x -> bad_type "float" x

    let string_of_jsonlike = function
      | Atd_jsonlike.AST.String (_, s) -> s
      | x -> bad_type "string" x

    let unit_of_jsonlike = function
      | Atd_jsonlike.AST.Null _ -> ()
      | x -> bad_type "null" x

    let list_of_jsonlike f = function
      | Atd_jsonlike.AST.Array (_, xs) -> List.map f xs
      | x -> bad_type "array" x

    let option_of_jsonlike f = function
      | Atd_jsonlike.AST.String (_, "None") -> None
      | Atd_jsonlike.AST.Array (_, [Atd_jsonlike.AST.String (_, "Some"); x]) -> Some (f x)
      | x -> bad_type "option" x

    let nullable_of_jsonlike f = function
      | Atd_jsonlike.AST.Null _ -> None
      | x -> Some (f x)

    let assoc_of_jsonlike f = function
      | Atd_jsonlike.AST.Object (_, pairs) ->
          List.map (fun (_, k, v) -> (k, f v)) pairs
      | x -> bad_type "object" x
  end|} in
  [B.Line (preamble
    ^ (if yojson then yojson_ext else "")
    ^ (if jsonlike then jsonlike_ext else "")
    ^ "\nend")]

(* ============ Type definition generation ============ *)

let gen_type_def ~is_mli env ({A.loc; name; param=params; annot=an; value=e; _} as def : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  let params_str = type_params_str params in
  let has_private = Atd.Annot.get_flag ~sections:["ocaml"] ~field:"private" an in
  let has_public  = Atd.Annot.get_flag ~sections:["ocaml"] ~field:"public"  an in
  if has_private && has_public then
    A.error_at loc "<ocaml private> and <ocaml public> are mutually exclusive";
  (* Decide whether to emit 'private' in the .mli:
     - primitive aliases are private by default (improves error messages and
       prevents direct construction), unless <ocaml public> suppresses it;
     - any type can be made private explicitly with <ocaml private>. *)
  let emit_private =
    is_mli &&
    ((primitive_alias_of_def def <> None && not has_public) || has_private)
  in
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
      let gen_case (loc, orig_name, an, opt_e) =
        let cons_name = vtr (get_ocaml_name orig_name an) in
        match opt_e with
        | None ->
            with_inline_doc (sprintf "| %s%s" tick cons_name) loc an
        | Some e ->
            with_inline_doc
              (sprintf "| %s%s of %s" tick cons_name (type_expr_str env e))
              loc an
      in
      let hd =
        if emit_private then
          sprintf "type %s%s = private" params_str ocaml_name
        else
          sprintf "type %s%s =" params_str ocaml_name
      in
      if is_poly then
        [
          B.Line (hd ^ " [");
          B.Block (concat_map gen_case flat);
          B.Line "]";
        ]
      else
        [
          B.Line hd;
          B.Block (concat_map gen_case flat);
        ]
  | Record (_, fields, rec_an) ->
      let prefix = get_field_prefix rec_an in
      let fields =
        List.filter_map (fun (f : field) -> match f with
          | Field x -> Some x
          | Inherit _ -> assert false
        ) fields
      in
      let (_, pftr) =
        make_prefixed_trs prefix (List.map (fun (_, (fname, _, _), _) -> fname) fields)
      in
      let private_kw = if emit_private then "private " else "" in
      [
        B.Line (sprintf "type %s%s = %s{" params_str ocaml_name private_kw);
        B.Block
          (concat_map
             (fun (loc, (fname, _, an), e) ->
               with_inline_doc
                 (sprintf "%s: %s;" (pftr fname) (type_expr_str env e))
                 loc an)
             fields);
        B.Line "}";
      ]
  | e ->
      let private_kw = if emit_private then "private " else "" in
      [B.Line (sprintf "type %s%s = %s%s" params_str ocaml_name private_kw (type_expr_str env e))]

(* ============ Creation function generation (records only) ============ *)

let get_implicit_default (e : type_expr) : string option =
  match e with
  | Name (_, (_, TN ["unit"], []), _) -> Some "()"
  | Name (_, (_, TN ["bool"], []), _) -> Some "false"
  | Name (_, (_, TN ["int"], []), _) -> Some "0"
  | Name (_, (_, TN ["float"], []), _) -> Some "0."
  | Name (_, (_, TN ["string"], []), _) -> Some {|""|}
  | List _ -> Some "[]"
  | Option _ -> Some "None"
  | Nullable _ -> Some "None"
  | _ -> None

(* Single warning pass over all type definitions, called once before codegen.
   Deduplicates by source location so that inherited fields warn only once. *)
let warn_defs (defs : A.type_def list) =
  let warned : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let once key f =
    if not (Hashtbl.mem warned key) then begin
      Hashtbl.add warned key ();
      f ()
    end
  in
  let warn_once loc msg =
    once (A.string_of_loc loc ^ ": " ^ msg)
      (fun () -> warn_not_supported loc msg)
  in
  let warn_once_raw loc msg =
    once (A.string_of_loc loc ^ ": " ^ msg)
      (fun () -> eprintf "Warning: %s: %s\n%!" (A.string_of_loc loc) msg)
  in
  List.iter (fun ({A.loc; name; annot=an; value=e; _} : A.type_def) ->
    let name = Atd.Type_name.basename name in
    (* Warn about <ocaml module="..."> / <ocaml t="..."> on type definitions *)
    (match get_ocaml_wrap an with
     | (Some _, _, _) | (_, Some _, _) | (_, _, Some _) ->
         warn_once loc
           "<ocaml module=\"...\"> / <ocaml t=\"...\"> on type definitions"
     | (None, None, None) -> ());
    (* Warn about field-level unsupported annotations and missing defaults *)
    (match e with
     | Record (_, fields, _) ->
         List.iter (fun (f : field) -> match f with
           | Inherit _ -> ()
           | Field (floc, (fname, kind, fan), fe) ->
               (* <ocaml name="..."> on record fields *)
               (match Atd.Annot.get_opt_field
                        ~parse:(fun s -> Some s)
                        ~sections:["ocaml"] ~field:"name" fan with
                | None -> ()
                | Some _ ->
                    warn_once floc "<ocaml name=\"...\"> on record fields");
               (* ~field without a determinable OCaml default *)
               (match kind with
                | Required | Optional -> ()
                | With_default ->
                    (match get_ocaml_default fan with
                     | Some _ -> ()
                     | None ->
                         (match get_implicit_default fe with
                          | Some _ -> ()
                          | None ->
                              warn_once_raw floc
                                (sprintf "field '%s' in type '%s' has no OCaml \
                                          default; create_%s will not be generated \
                                          and the field will be required in JSON"
                                   fname name name))))
         ) fields
     | _ -> ())
  ) defs

let gen_make_fun env ({A.name; param=params; value=e; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  match e with
  | Record (_, fields, rec_an) ->
      let prefix = get_field_prefix rec_an in
      let fields =
        List.filter_map (fun (f : field) -> match f with
          | Field x -> Some x
          | Inherit _ -> assert false
        ) fields
      in
      let fnames = List.map (fun (_, (fname, _, _), _) -> fname) fields in
      let (label_tr, pftr) = make_prefixed_trs prefix fnames in
      (* Try to resolve the default for a With_default field.
         Returns None if no default can be determined, in which case
         we skip generating make_* for this type. *)
      let try_get_default (_, (fname, kind, an), e) =
        match kind with
        | Required | Optional -> Some None
        | With_default ->
            match get_ocaml_default an with
            | Some d -> Some (Some d)
            | None ->
                match get_implicit_default e with
                | Some d -> Some (Some d)
                | None ->
                    None
      in
      let resolved = List.map try_get_default fields in
      if List.exists (fun r -> r = None) resolved then []
      else
      let gen_param (_, (fname, kind, _), _) default_opt =
        let lname = label_tr fname in
        match kind with
        | Required -> sprintf "~%s" lname
        | Optional -> sprintf "?%s" lname
        | With_default ->
            let default = Option.get (Option.get default_opt) in
            sprintf "?(%s = %s)" lname default
      in
      let param_strs = List.map2 gen_param fields resolved in
      (* Labels are unprefixed; record field names carry the prefix.
         Emit 'pfname = lname' when they differ, else use shorthand. *)
      let field_assigns =
        List.map (fun (_, (fname, _, _), _) ->
          let lname = label_tr fname in
          let pfname = pftr fname in
          if pfname = lname then pfname
          else sprintf "%s = %s" pfname lname
        ) fields
      in
      [
        B.Line
          (sprintf "let %s %s () : %s ="
             (create_name ocaml_name)
             (String.concat " " param_strs)
             (full_type_name ocaml_name params));
        B.Block
          [B.Line (sprintf "{ %s }" (String.concat "; " field_assigns))];
      ]
  | _ -> []

(* ============ Creation functions for primitive aliases ============ *)

(* For 'type foo = string' (or bool/int/float/unit), emit:
     let create_foo (x : string) : foo = x
   In the .ml the types are transparent (no 'private'), so this is an
   identity function.  Its value is the type-annotated API it exposes,
   which matches the opaque 'private' interface in the .mli.
   Coercion back to the primitive is done with ':>' and is not generated. *)
let gen_alias_create_funs env (def : A.type_def) : B.t =
  let name = Atd.Type_name.basename def.A.name in
  let ocaml_name = env.tr name in
  match primitive_alias_of_def def with
  | None -> []
  | Some prim ->
      let prim_ocaml = builtin_ocaml_name prim in
      [
        B.Line (sprintf "let %s (x : %s) : %s = x"
                  (create_name ocaml_name) prim_ocaml ocaml_name);
        B.Line "";
      ]

(* ============ Deserialization function generation ============ *)

let gen_reader_field env ftr mode type_name (loc, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  let ofname = ftr fname in
  let match_nodes =
    match kind with
    | Required ->
        [
          B.Line (sprintf "match assoc_ \"%s\" with" json_name);
          B.Line (sprintf "| Some v -> %s v" (reader_expr env mode e));
          B.Line (sprintf "| None -> %s" (mode.missing_req_expr type_name json_name));
        ]
    | Optional ->
        let inner_e =
          match e with
          | Option (_, ie, _) -> ie
          | _ -> e
        in
        [
          B.Line (sprintf "match assoc_ \"%s\" with" json_name);
          B.Line (sprintf "| %s -> None" mode.optional_null_pat);
          B.Line (sprintf "| Some v -> Some (%s v)" (reader_expr env mode inner_e));
        ]
    | With_default ->
        (match get_ocaml_default an with
        | Some default ->
            [
              B.Line (sprintf "match assoc_ \"%s\" with" json_name);
              B.Line (sprintf "| None -> %s" default);
              B.Line (sprintf "| Some v -> %s v" (reader_expr env mode e));
            ]
        | None ->
            match get_implicit_default e with
            | Some default ->
                [
                  B.Line (sprintf "match assoc_ \"%s\" with" json_name);
                  B.Line (sprintf "| None -> %s" default);
                  B.Line (sprintf "| Some v -> %s v" (reader_expr env mode e));
                ]
            | None ->
                (* No OCaml default — treat as required; warned by warn_defs *)
                [
                  B.Line (sprintf "match assoc_ \"%s\" with" json_name);
                  B.Line (sprintf "| Some v -> %s v" (reader_expr env mode e));
                  B.Line (sprintf "| None -> %s" (mode.missing_req_expr type_name json_name));
                ])
  in
  B.Inline
    [
      B.Line (sprintf "let %s =" ofname);
      B.Block match_nodes;
      B.Line "in";
    ]

let gen_reader env mode ({A.name; param=params; annot=an; value=e; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  let param_strs = List.map mode.mode_tvar_reader params in
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
        let sum = Atd.Json.get_json_sum sum_an in
        let tagged_pat = match sum.json_sum_repr with
          | Atd.Json.Array  -> mode.sum_tagged_pat
          | Atd.Json.Object -> mode.sum_object_tagged_pat
        in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let cons_name = vtr (get_ocaml_name orig_name an) in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| %s -> %s%s"
                   (mode.sum_unit_pat json_name) tick cons_name)
          | Some e ->
              B.Line
                (sprintf "| %s -> %s%s (%s v)"
                   (tagged_pat json_name) tick cons_name
                   (reader_expr env mode e))
        in
        let normalize =
          mode.normalize_of_adapter sum.json_sum_adapter
        in
        let pre = match normalize with
          | None -> []
          | Some f -> [B.Line (sprintf "let x = %s x in" f)]
        in
        B.Block
          (pre
           @ [B.Line "match x with"]
           @ List.map gen_case flat
           @ [B.Line (sprintf "| _ -> %s.bad_sum \"%s\" x" mode.runtime name)])
    | Record (_, fields, rec_an) ->
        let prefix = get_field_prefix rec_an in
        let fields =
          List.filter_map (fun (f : field) -> match f with
            | Field x -> Some x
            | Inherit _ -> assert false)
            fields
        in
        let fnames = List.map (fun (_, (fname, _, _), _) -> fname) fields in
        let (label_tr, pftr) = make_prefixed_trs prefix fnames in
        (* Local variable bindings use label_tr (unprefixed, keyword-safe).
           The record literal uses pftr (prefix applied before keyword check).
           Emit 'pfname = lname' when they differ, else use shorthand. *)
        let field_assigns =
          List.map (fun (_, (fname, _, _), _) ->
            let lname = label_tr fname in
            let pfname = pftr fname in
            if pfname = lname then pfname
            else sprintf "%s = %s" pfname lname
          ) fields
        in
        let normalize =
          mode.normalize_of_adapter (Atd.Json.get_json_record rec_an).json_record_adapter
        in
        let pre = match normalize with
          | None -> []
          | Some f -> [B.Line (sprintf "let x = %s x in" f)]
        in
        let match_block =
          B.Block
            (pre
             @ [ B.Line "match x with";
                 B.Line (sprintf "| %s ->" mode.record_match_pat);
                 B.Block
                   (mode.record_node_binding
                    @ mode.record_assoc_lines
                    @ List.map (gen_reader_field env label_tr mode name) fields
                    @ [B.Line (sprintf "{ %s }" (String.concat "; " field_assigns))]);
                 B.Line (sprintf "| _ -> %s.bad_type \"%s\" x" mode.runtime name) ])
        in
        match_block
    | e ->
        B.Block [B.Line (sprintf "%s x" (reader_expr env mode e))]
  in
  (* For parametric functions we use explicit universal quantification so that
     OCaml treats each function as polymorphic within the let rec...and block.
     Without this, a use site like 'result_of_yojson int_of_yojson' would fix
     the type of 'result_of_yojson' for the entire block. *)
  if params = [] then
    [
      B.Line (sprintf "let %s (x : %s) : %s ="
                (mode.of_name ocaml_name) mode.tree_type return_type);
      body;
    ]
  else
    let quant = String.concat " " (List.map (fun v -> "'" ^ v) params) in
    let param_types =
      String.concat " "
        (List.map (fun v -> sprintf "(%s -> '%s) ->" mode.tree_type v) params)
    in
    let full_type = sprintf "%s %s -> %s" param_types mode.tree_type return_type in
    [
      B.Line (sprintf "let %s : %s. %s =" (mode.of_name ocaml_name) quant full_type);
      B.Block [B.Line (sprintf "fun %sx ->" extra_params); body];
    ]

(* ============ Serialization function generation ============ *)

(* [pftr] maps the ATD field name to the prefixed OCaml field name
   used in the record type definition (e.g. "pre_" ^ ftr fname). *)
let gen_yojson_of_field env pftr (_, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  let ofname = pftr fname in
  match kind with
  | Required | With_default ->
      B.Line (sprintf "[(\"%s\", %s x.%s)];" json_name (writer_expr env e) ofname)
  | Optional ->
      let inner_e =
        match e with
        | Option (_, ie, _) -> ie
        | _ -> e
      in
      B.Line
        (sprintf
           "(match x.%s with None -> [] | Some v -> [(\"%s\", %s v)]);"
           ofname json_name (writer_expr env inner_e))

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

let gen_yojson_of env ({A.name; param=params; annot=an; value=e; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
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
        let sum = Atd.Json.get_json_sum sum_an in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let cons_name = vtr (get_ocaml_name orig_name an) in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| %s%s -> `String \"%s\""
                   tick cons_name json_name)
          | Some e ->
              let rhs = match sum.json_sum_repr with
                | Atd.Json.Array ->
                    sprintf "`List [`String \"%s\"; %s v]" json_name (writer_expr env e)
                | Atd.Json.Object ->
                    sprintf "`Assoc [(\"%s\", %s v)]" json_name (writer_expr env e)
              in
              B.Line (sprintf "| %s%s v -> %s" tick cons_name rhs)
        in
        let (_, restore) =
          adapter_exprs sum.json_sum_adapter
        in
        apply_restore restore
          (B.Block
            (B.Line "match x with"
             :: List.map gen_case flat))
    | Record (_, fields, rec_an) ->
        let prefix = get_field_prefix rec_an in
        let fields =
          List.filter_map (fun (f : field) -> match f with
            | Field x -> Some x
            | Inherit _ -> assert false)
            fields
        in
        let fnames = List.map (fun (_, (fname, _, _), _) -> fname) fields in
        (* pftr maps ATD field names to the prefixed OCaml record field names,
           with the prefix applied before keyword checking. *)
        let (_, pftr) = make_prefixed_trs prefix fnames in
        let (_, restore) =
          adapter_exprs (Atd.Json.get_json_record rec_an).json_record_adapter
        in
        apply_restore restore
          (B.Block
            [
              B.Line "`Assoc (List.concat [";
              B.Block (List.map (gen_yojson_of_field env pftr) fields);
              B.Line "])";
            ])
    | e ->
        B.Block [B.Line (sprintf "%s x" (writer_expr env e))]
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

let gen_io_funs env ({A.name; param=params; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
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

let gen_submodule_ml ~yojson ~jsonlike env ({A.name; param=params; annot=def_an; value=e; _} as def : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  let params_str = type_params_str params in
  let type_decl =
    sprintf "type nonrec %st = %s%s" params_str (type_params_str params) ocaml_name
  in
  let attr_lines =
    match get_ocaml_attr def_an with
    | None -> []
    | Some attr -> [B.Line (sprintf "[@@%s]" attr)]
  in
  let create_binding =
    match e with
    | Record _ ->
        [B.Line (sprintf "let create = %s" (create_name ocaml_name))]
    | _ ->
        (match primitive_alias_of_def def with
         | None -> []
         | Some _ ->
             [B.Line (sprintf "let create = %s" (create_name ocaml_name))])
  in
  let bindings =
    create_binding
    @ (if yojson then [B.Line (sprintf "let of_yojson = %s" (of_yojson_name ocaml_name))] else [])
    @ (if jsonlike then [B.Line (sprintf "let of_jsonlike = %s" (of_jsonlike_name ocaml_name))] else [])
    @ (if yojson then [
        B.Line (sprintf "let to_yojson = %s" (yojson_of_name ocaml_name));
        B.Line (sprintf "let of_json = %s" (of_json_name ocaml_name));
        B.Line (sprintf "let to_json = %s" (json_of_name ocaml_name));
      ] else [])
  in
  [
    B.Line (sprintf "module %s = struct" (module_name ocaml_name));
    B.Block (B.Line type_decl :: attr_lines @ bindings);
    B.Line "end";
  ]

(* ============ MLI: type signatures ============ *)

let gen_reader_sig env mode ({A.name; param=params; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  match params with
  | [] ->
      [B.Line (sprintf "val %s : %s -> %s"
                 (mode.of_name ocaml_name) mode.tree_type ocaml_name)]
  | _ ->
      let param_sigs =
        List.map (fun v -> sprintf "(%s -> '%s) ->" mode.tree_type v) params
      in
      let return_type = full_type_name ocaml_name params in
      [
        B.Line (sprintf "val %s :" (mode.of_name ocaml_name));
        B.Block
          (List.map (fun s -> B.Line s) param_sigs
           @ [B.Line (mode.tree_type ^ " ->"); B.Line return_type]);
      ]

let gen_yojson_of_sig env ({A.name; param=params; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
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

let gen_make_sig env ({A.name; param=params; value=e; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
  match e with
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (fun (f : field) -> match f with
          | Field x -> Some x
          | Inherit _ -> assert false)
          fields
      in
      let ftr =
        make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
      in
      let gen_param_sig (loc, (fname, kind, an), e) =
        let ofname = ftr fname in
        match kind with
        | Required -> sprintf "%s:%s ->" ofname (type_expr_str env e)
        | Optional ->
            let inner =
              match e with
              | Option (_, ie, _) -> ie
              | _ -> e
            in
            sprintf "?%s:%s ->" ofname (type_expr_str env inner)
        | With_default -> sprintf "?%s:%s ->" ofname (type_expr_str env e)
      in
      let param_sigs = List.map gen_param_sig fields in
      let return_type = full_type_name ocaml_name params in
      [
        B.Line
          (sprintf "val %s : %sunit -> %s"
             (create_name ocaml_name)
             (String.concat " " param_sigs ^ " ")
             return_type);
      ]
  | _ -> []

(* Signature for the create function of primitive aliases. *)
let gen_alias_create_sigs env (def : A.type_def) : B.t =
  let name = Atd.Type_name.basename def.A.name in
  let ocaml_name = env.tr name in
  match primitive_alias_of_def def with
  | None -> []
  | Some prim ->
      let prim_ocaml = builtin_ocaml_name prim in
      [
        B.Line (sprintf "val %s : %s -> %s"
                  (create_name ocaml_name) prim_ocaml ocaml_name);
      ]

let gen_io_sigs env ({A.name; param=params; _} : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
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

let gen_submodule_mli ~yojson ~jsonlike env ({A.name; param=params; annot=def_an; value=e; _} as def : A.type_def) : B.t =
  let name = Atd.Type_name.basename name in
  let ocaml_name = env.tr name in
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
  let create_sig =
    match e with
    | Record (_, fields, _) ->
        let fields =
          List.filter_map (fun (f : field) -> match f with
            | Field x -> Some x
            | Inherit _ -> assert false)
            fields
        in
        let ftr =
          make_local_env (List.map (fun (_, (fname, _, _), _) -> fname) fields)
        in
        let gen_param_sig (loc, (fname, kind, an), e) =
          let ofname = ftr fname in
          match kind with
          | Required -> sprintf "%s:%s ->" ofname (type_expr_str env e)
          | Optional ->
              let inner =
                match e with
                | Option (_, ie, _) -> ie
                | _ -> e
              in
              sprintf "?%s:%s ->" ofname (type_expr_str env inner)
          | With_default -> sprintf "?%s:%s ->" ofname (type_expr_str env e)
        in
        let param_sigs = List.map gen_param_sig fields in
        [
          B.Line
            (sprintf "val create : %sunit -> %s"
               (String.concat " " param_sigs ^ " ")
               t_type);
        ]
    | _ ->
        (match primitive_alias_of_def def with
         | None -> []
         | Some prim ->
             let prim_ocaml = builtin_ocaml_name prim in
             [B.Line (sprintf "val create : %s -> %s" prim_ocaml t_type)])
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
  let of_jsonlike_sig =
    match params with
    | [] -> [B.Line (sprintf "val of_jsonlike : Atd_jsonlike.AST.t -> %s" t_type)]
    | _ ->
        let param_sigs =
          List.map (fun v -> sprintf "(Atd_jsonlike.AST.t -> '%s) ->" v) params
        in
        [
          B.Line "val of_jsonlike :";
          B.Block
            (List.map (fun s -> B.Line s) param_sigs
             @ [B.Line "Atd_jsonlike.AST.t ->"; B.Line t_type]);
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
    @ create_sig
    @ (if yojson then of_yojson_sig else [])
    @ (if jsonlike then of_jsonlike_sig else [])
    @ (if yojson then to_yojson_sig @ of_json_sig @ to_json_sig else [])
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
let emit_type_group ~is_mli env (defs : A.type_def list) : B.t =
  let attr_line ({A.annot=def_an; _} : A.type_def) =
    match get_ocaml_attr def_an with
    | None -> []
    | Some attr -> [B.Line (sprintf "[@@%s]" attr)]
  in
  let doc_lines ({A.loc; annot=def_an; _} : A.type_def) : B.t =
    doc_comment_prepend loc def_an
  in
  match defs with
  | [] -> []
  | [def] -> doc_lines def @ gen_type_def ~is_mli env def @ attr_line def @ [B.Line ""]
  | first :: rest ->
      let first_lines = doc_lines first @ gen_type_def ~is_mli env first @ attr_line first in
      let rest_lines =
        concat_map (fun def ->
          let lines =
            match gen_type_def ~is_mli env def with
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

let make_ml ~yojson ~jsonlike ~imports ~env ~atd_filename ~module_doc (items : A.type_def list) : B.t =
  (* In .ml, use the annotation-aware alias as the OCaml module name,
     and emit 'module Alias = OcamlMod' for any import where they differ. *)
  let env = { env with ocaml_mod_of_import = ocaml_name_of_import } in
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
    let defs = group_items in
    let types = emit_type_group ~is_mli:false env defs in
    let creates =
      concat_map (fun def ->
        let lines = gen_make_fun env def @ gen_alias_create_funs env def in
        if lines = [] then [] else lines @ [B.Line ""]
      ) defs
    in
    let of_yojsons = if yojson then emit_fun_group ~is_recursive (gen_reader env yojson_mode) defs else [] in
    let of_jsonlikes = if jsonlike then emit_fun_group ~is_recursive (gen_reader env jsonlike_mode) defs else [] in
    let yojson_ofs = if yojson then emit_fun_group ~is_recursive (gen_yojson_of env) defs else [] in
    let ios =
      if yojson then concat_map (fun def -> gen_io_funs env def @ [B.Line ""]) defs
      else []
    in
    let submods =
      concat_map (fun def -> gen_submodule_ml ~yojson ~jsonlike env def @ [B.Line ""]) defs
    in
    types @ creates @ of_yojsons @ of_jsonlikes @ yojson_ofs @ ios @ submods
  in
  let aliases = module_alias_decls imports in
  header
  @ runtime_module ~yojson ~jsonlike
  @ [B.Line ""]
  @ (if aliases = [] then [] else aliases @ [B.Line ""])
  @ concat_map gen_group (Atd.Util.tsort items)

(* ============ Assemble the .mli file ============ *)

let make_mli ~yojson ~jsonlike ~env ~atd_filename ~module_doc (items : A.type_def list) : B.t =
  (* In .mli, always use the natural OCaml module name (last path component,
     capitalized).  No alias declarations are emitted in .mli. *)
  let env = { env with ocaml_mod_of_import = ocaml_module_of_import } in
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
    let defs = group_items in
    let types = emit_type_group ~is_mli:true env defs in
    let sigs =
      concat_map (fun def ->
        gen_make_sig env def
        @ gen_alias_create_sigs env def
        @ (if yojson then gen_reader_sig env yojson_mode def else [])
        @ (if jsonlike then gen_reader_sig env jsonlike_mode def else [])
        @ (if yojson then gen_yojson_of_sig env def @ gen_io_sigs env def else [])
        @ [B.Line ""]
      ) defs
    in
    let submod_sigs =
      concat_map (fun def -> gen_submodule_mli ~yojson ~jsonlike env def @ [B.Line ""]) defs
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
let run_stdin ~yojson ~jsonlike () =
  let module_ =
    Atd.Util.read_channel
      ~annot_schema
      ~expand:false
      ~keep_builtins:false
      ~inherit_fields:true
      ~inherit_variants:true
      stdin
  in
  let (head_loc, head_an) = module_.A.module_head in
  let atd_module = module_.A.type_defs in
  let module_doc = doc_comment_prepend head_loc head_an in
  let defs = atd_module in
  let () = warn_defs defs in
  let imports = module_.A.imports in
  let env = init_env imports defs in
  let mli = make_mli ~yojson ~jsonlike ~env ~atd_filename:"<stdin>" ~module_doc atd_module in
  let ml = make_ml ~yojson ~jsonlike ~imports ~env ~atd_filename:"<stdin>" ~module_doc atd_module in
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

let run_file ~yojson ~jsonlike src_path =
  let src_name = Filename.basename src_path in
  let base_name =
    if Filename.check_suffix src_name ".atd" then
      Filename.chop_suffix src_name ".atd"
    else
      src_name
  in
  let ml_path = String.lowercase_ascii base_name ^ ".ml" in
  let mli_path = String.lowercase_ascii base_name ^ ".mli" in
  let module_ =
    Atd.Util.load_file
      ~annot_schema
      ~expand:false
      ~keep_builtins:false
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let (head_loc, head_an) = module_.A.module_head in
  let atd_module = module_.A.type_defs in
  let module_doc = doc_comment_prepend head_loc head_an in
  let defs = atd_module in
  let () = warn_defs defs in
  let imports = module_.A.imports in
  let env = init_env imports defs in
  let ml_contents = make_ml ~yojson ~jsonlike ~imports ~env ~atd_filename:src_name ~module_doc atd_module in
  let mli_contents = make_mli ~yojson ~jsonlike ~env ~atd_filename:src_name ~module_doc atd_module in
  B.to_file ~indent:2 ml_path ml_contents;
  B.to_file ~indent:2 mli_path mli_contents
