(*
   OCaml code generation for JSON support using the Yojson AST.

   For each ATD type 'foo', the generated code contains:
   - A type definition 'type foo = ...'
   - A creation function 'let make_foo ...' (for record types only)
   - A deserialization function 'let foo_of_yojson ...'
   - A serialization function 'let yojson_of_foo ...'
   - Top-level I/O functions (for non-parametrized types only)

   Two files are produced: 'foo.ml' and 'foo.mli'.

   Design notes:
   - Uses Yojson.Safe.t as the intermediate JSON representation,
     not the semi-secret streaming parser functions used by atdgen.
   - Generates classic OCaml variants by default; use <ocaml repr="poly">
     on the sum type to get polymorphic variants instead.
   - No separate _t.ml / _j.ml split: one ATD file → one OCaml module.
   - Naming convention: foo_of_yojson / yojson_of_foo (same as
     ppx_yojson_conv, compatible with ppx_deriving.yojson).
*)

open Printf
open Atd.Ast
module A = Atd.Ast
module B = Indent

(* ============ Annotation schema ============ *)

let annot_schema_ml : Atd.Annot.schema_section = {
  section = "ml";
  fields = [
    Field, "default";
  ]
}

let annot_schema : Atd.Annot.schema =
  annot_schema_ml :: Atd.Json.annot_schema_json

(* ============ Errors ============ *)

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdml: " ^ msg)

(* ============ Annotation helpers ============ *)

(* Get <ml default="..."> or <ocaml default="..."> for with-default fields *)
let get_ml_default an =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["ml"; "ocaml"]
    ~field:"default"
    an

(* Get <ocaml repr="poly"> to request polymorphic variants *)
let get_ocaml_repr an =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["ocaml"]
    ~field:"repr"
    an

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
let make_name name = "make_" ^ name

(* Function parameter name for reading/writing a type variable *)
let tvar_reader v = "of_yojson_" ^ v
let tvar_writer v = "yojson_of_" ^ v

(* ============ Builtin OCaml type names ============ *)

let builtin_ocaml_name = function
  | "unit" -> "unit"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "float"
  | "string" -> "string"
  | "abstract" -> "Yojson.Safe.t"
  | name -> name

(* ============ OCaml type expression from ATD type ============ *)

let rec type_expr_str (e : type_expr) : string =
  match e with
  | Sum _ | Record _ -> failwith "inline types are not supported"
  | Tuple (_, cells, _) ->
      let parts = List.map (fun (_, e, _) -> type_expr_str e) cells in
      "(" ^ String.concat " * " parts ^ ")"
  | List (_, e, _) -> type_expr_str e ^ " list"
  | Option (_, e, _) -> type_expr_str e ^ " option"
  | Nullable (_, e, _) -> type_expr_str e ^ " option"
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (loc, _, _) -> not_implemented loc "wrap"
  | Name (_, (_, name, []), _) -> builtin_ocaml_name name
  | Name (_, (_, name, params), _) ->
      let pstrs = List.map type_expr_str params in
      "(" ^ String.concat ", " pstrs ^ ") " ^ name
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

let rec reader_expr (e : type_expr) : string =
  match e with
  | Name (_, (_, "unit", []), _) -> "Atdml_runtime.unit_of_yojson"
  | Name (_, (_, "bool", []), _) -> "Atdml_runtime.bool_of_yojson"
  | Name (_, (_, "int", []), _) -> "Atdml_runtime.int_of_yojson"
  | Name (_, (_, "float", []), _) -> "Atdml_runtime.float_of_yojson"
  | Name (_, (_, "string", []), _) -> "Atdml_runtime.string_of_yojson"
  | Name (_, (_, "abstract", []), _) -> "(fun x -> x)"
  | Name (_, (_, name, []), _) -> of_yojson_name name
  | Name (_, (_, name, params), _) ->
      let readers = List.map reader_expr params in
      sprintf "(%s %s)" (of_yojson_name name) (String.concat " " readers)
  | List (_, e, _) ->
      sprintf "(Atdml_runtime.list_of_yojson %s)" (reader_expr e)
  | Option (_, e, _) ->
      sprintf "(Atdml_runtime.option_of_yojson %s)" (reader_expr e)
  | Nullable (_, e, _) ->
      sprintf "(Atdml_runtime.nullable_of_yojson %s)" (reader_expr e)
  | Tuple (_, cells, _) ->
      let n = List.length cells in
      let reads =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s (List.nth lst %d)" (reader_expr e) i
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
  | Wrap (loc, _, _) -> not_implemented loc "wrap"
  | Sum _ | Record _ -> failwith "inline types are not supported"

(* ============ Writer function expressions ============ *)
(*
   Returns an OCaml expression of type ('a -> Yojson.Safe.t).
   These are used as arguments to combinators like yojson_of_list.
*)

let rec writer_expr (e : type_expr) : string =
  match e with
  | Name (_, (_, "unit", []), _) -> "Atdml_runtime.yojson_of_unit"
  | Name (_, (_, "bool", []), _) -> "Atdml_runtime.yojson_of_bool"
  | Name (_, (_, "int", []), _) -> "Atdml_runtime.yojson_of_int"
  | Name (_, (_, "float", []), _) -> "Atdml_runtime.yojson_of_float"
  | Name (_, (_, "string", []), _) -> "Atdml_runtime.yojson_of_string"
  | Name (_, (_, "abstract", []), _) -> "(fun x -> x)"
  | Name (_, (_, name, []), _) -> yojson_of_name name
  | Name (_, (_, name, params), _) ->
      let writers = List.map writer_expr params in
      sprintf "(%s %s)" (yojson_of_name name) (String.concat " " writers)
  | List (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_list %s)" (writer_expr e)
  | Option (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_option %s)" (writer_expr e)
  | Nullable (_, e, _) ->
      sprintf "(Atdml_runtime.yojson_of_nullable %s)" (writer_expr e)
  | Tuple (_, cells, _) ->
      let vars = List.mapi (fun i _ -> sprintf "x%d" i) cells in
      let writes =
        List.mapi (fun i (_, e, _) ->
          sprintf "%s %s" (writer_expr e) (List.nth vars i)
        ) cells
      in
      sprintf "(fun (%s) -> `List [%s])"
        (String.concat ", " vars)
        (String.concat "; " writes)
  | Tvar (_, v) -> tvar_writer v
  | Shared (loc, _, _) -> not_implemented loc "shared"
  | Wrap (loc, _, _) -> not_implemented loc "wrap"
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

let gen_type_def ((loc, (name, params, an), e) : A.type_def) : B.t =
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
      let gen_case (_, orig_name, an, opt_e) =
        let ocaml_name = get_ocaml_name orig_name an in
        match opt_e with
        | None ->
            B.Line (sprintf "| %s%s" tick ocaml_name)
        | Some e ->
            B.Line (sprintf "| %s%s of %s" tick ocaml_name (type_expr_str e))
      in
      if is_poly then
        [
          B.Line (sprintf "type %s%s = [" params_str name);
          B.Block (List.map gen_case flat);
          B.Line "]";
        ]
      else
        [
          B.Line (sprintf "type %s%s =" params_str name);
          B.Block (List.map gen_case flat);
        ]
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false
        ) fields
      in
      [
        B.Line (sprintf "type %s%s = {" params_str name);
        B.Block
          (List.map
             (fun (_, (fname, _, _), e) ->
               B.Line (sprintf "%s: %s;" fname (type_expr_str e)))
             fields);
        B.Line "}";
      ]
  | e ->
      [B.Line (sprintf "type %s%s = %s" params_str name (type_expr_str e))]

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

let gen_make_fun ((loc, (name, params, an), e) : A.type_def) : B.t =
  match e with
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false
        ) fields
      in
      let gen_param (_, (fname, kind, an), e) =
        match kind with
        | Required -> sprintf "~%s" fname
        | Optional -> sprintf "?%s" fname
        | With_default ->
            let default =
              match get_ml_default an with
              | Some d -> d
              | None ->
                  match get_implicit_default e with
                  | Some d -> d
                  | None ->
                      A.error_at loc
                        (sprintf
                           "field '%s' needs a default value; \
                            use <ml default=\"...\"> annotation"
                           fname)
            in
            sprintf "?(%s = %s)" fname default
      in
      let param_strs = List.map gen_param fields in
      let field_names =
        List.map (fun (_, (fname, _, _), _) -> fname) fields
      in
      [
        B.Line
          (sprintf "let %s %s () : %s ="
             (make_name name)
             (String.concat " " param_strs)
             (full_type_name name params));
        B.Block
          [B.Line (sprintf "{ %s }" (String.concat "; " field_names))];
      ]
  | _ -> []

(* ============ Deserialization function generation ============ *)

let gen_of_yojson_field type_name (loc, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  let match_nodes =
    match kind with
    | Required ->
        [
          B.Line (sprintf "match List.assoc_opt \"%s\" fields with" json_name);
          B.Line (sprintf "| Some v -> %s v" (reader_expr e));
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
          B.Line (sprintf "| Some v -> Some (%s v)" (reader_expr inner_e));
        ]
    | With_default ->
        let default =
          match get_ml_default an with
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
          B.Line (sprintf "| Some v -> %s v" (reader_expr e));
        ]
  in
  B.Inline
    [
      B.Line (sprintf "let %s =" fname);
      B.Block match_nodes;
      B.Line "in";
    ]

let gen_of_yojson ((loc, (name, params, an), e) : A.type_def) : B.t =
  let param_strs =
    List.map
      (fun v -> sprintf "(of_yojson_%s : Yojson.Safe.t -> '%s)" v v)
      params
  in
  let extra_params =
    if param_strs = [] then ""
    else String.concat " " param_strs ^ " "
  in
  let return_type = full_type_name name params in
  let body =
    match e with
    | Sum (_, variants, an) ->
        let is_poly =
          match get_ocaml_repr an with
          | Some "poly" -> true
          | _ -> false
        in
        let tick = if is_poly then "`" else "" in
        let flat = flatten_variants variants in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let ocaml_name = get_ocaml_name orig_name an in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| `String \"%s\" -> %s%s"
                   json_name tick ocaml_name)
          | Some e ->
              B.Line
                (sprintf "| `List [`String \"%s\"; v] -> %s%s (%s v)"
                   json_name tick ocaml_name (reader_expr e))
        in
        B.Block
          (B.Line "match x with"
           :: List.map gen_case flat
           @ [B.Line (sprintf "| _ -> Atdml_runtime.bad_sum \"%s\" x" name)])
    | Record (_, fields, _) ->
        let fields =
          List.filter_map (function
            | `Field x -> Some x
            | `Inherit _ -> assert false)
            fields
        in
        let field_names =
          List.map (fun (_, (fname, _, _), _) -> fname) fields
        in
        B.Block
          [
            B.Line "match x with";
            B.Line "| `Assoc fields ->";
            B.Block
              (List.map (gen_of_yojson_field name) fields
               @ [B.Line (sprintf "{ %s }" (String.concat "; " field_names))]);
            B.Line (sprintf "| _ -> Atdml_runtime.bad_type \"%s\" x" name);
          ]
    | e ->
        B.Block [B.Line (sprintf "%s x" (reader_expr e))]
  in
  [
    B.Line
      (sprintf "let %s %s(x : Yojson.Safe.t) : %s ="
         (of_yojson_name name) extra_params return_type);
    body;
  ]

(* ============ Serialization function generation ============ *)

let gen_yojson_of_field (_, (fname, kind, an), e) : B.node =
  let json_name = Atd.Json.get_json_fname fname an in
  match kind with
  | Required | With_default ->
      B.Line (sprintf "[(\"%s\", %s x.%s)];" json_name (writer_expr e) fname)
  | Optional ->
      let inner_e =
        match e with
        | Option (_, ie, _) -> ie
        | _ -> e
      in
      B.Line
        (sprintf
           "(match x.%s with None -> [] | Some v -> [(\"%s\", %s v)]);"
           fname json_name (writer_expr inner_e))

let gen_yojson_of ((loc, (name, params, an), e) : A.type_def) : B.t =
  let param_strs =
    List.map
      (fun v -> sprintf "(yojson_of_%s : '%s -> Yojson.Safe.t)" v v)
      params
  in
  let extra_params =
    if param_strs = [] then ""
    else String.concat " " param_strs ^ " "
  in
  let arg_type = full_type_name name params in
  let body =
    match e with
    | Sum (_, variants, an) ->
        let is_poly =
          match get_ocaml_repr an with
          | Some "poly" -> true
          | _ -> false
        in
        let tick = if is_poly then "`" else "" in
        let flat = flatten_variants variants in
        let gen_case (_, orig_name, an, opt_e) =
          let json_name = Atd.Json.get_json_cons orig_name an in
          let ocaml_name = get_ocaml_name orig_name an in
          match opt_e with
          | None ->
              B.Line
                (sprintf "| %s%s -> `String \"%s\""
                   tick ocaml_name json_name)
          | Some e ->
              B.Line
                (sprintf "| %s%s v -> `List [`String \"%s\"; %s v]"
                   tick ocaml_name json_name (writer_expr e))
        in
        B.Block
          (B.Line "match x with"
           :: List.map gen_case flat)
    | Record (_, fields, _) ->
        let fields =
          List.filter_map (function
            | `Field x -> Some x
            | `Inherit _ -> assert false)
            fields
        in
        B.Block
          [
            B.Line "`Assoc (List.concat [";
            B.Block (List.map gen_yojson_of_field fields);
            B.Line "])";
          ]
    | e ->
        B.Block [B.Line (sprintf "%s x" (writer_expr e))]
  in
  [
    B.Line
      (sprintf "let %s %s(x : %s) : Yojson.Safe.t ="
         (yojson_of_name name) extra_params arg_type);
    body;
  ]

(* ============ Top-level I/O functions (non-parametrized only) ============ *)

let gen_io_funs name : B.t =
  [
    B.Line (sprintf "let %s_of_string s =" name);
    B.Block [B.Line (sprintf "%s (Yojson.Safe.from_string s)" (of_yojson_name name))];
    B.Line (sprintf "let string_of_%s x =" name);
    B.Block [B.Line (sprintf "Yojson.Safe.to_string (%s x)" (yojson_of_name name))];
    B.Line (sprintf "let %s_of_channel ic =" name);
    B.Block [B.Line (sprintf "%s (Yojson.Safe.from_channel ic)" (of_yojson_name name))];
    B.Line (sprintf "let %s_of_file path =" name);
    B.Block [B.Line (sprintf "%s (Yojson.Safe.from_file path)" (of_yojson_name name))];
  ]

(* ============ MLI: type signatures ============ *)

let gen_of_yojson_sig ((_, (name, params, _), _) : A.type_def) : B.t =
  match params with
  | [] ->
      [B.Line (sprintf "val %s : Yojson.Safe.t -> %s" (of_yojson_name name) name)]
  | _ ->
      let param_sigs = List.map (fun v -> sprintf "(Yojson.Safe.t -> '%s) ->" v) params in
      let return_type = full_type_name name params in
      [
        B.Line (sprintf "val %s :" (of_yojson_name name));
        B.Block
          (List.map (fun s -> B.Line s) param_sigs
           @ [B.Line "Yojson.Safe.t ->"; B.Line return_type]);
      ]

let gen_yojson_of_sig ((_, (name, params, _), _) : A.type_def) : B.t =
  match params with
  | [] ->
      [B.Line (sprintf "val %s : %s -> Yojson.Safe.t" (yojson_of_name name) name)]
  | _ ->
      let param_sigs = List.map (fun v -> sprintf "('%s -> Yojson.Safe.t) ->" v) params in
      let arg_type = full_type_name name params in
      [
        B.Line (sprintf "val %s :" (yojson_of_name name));
        B.Block
          (List.map (fun s -> B.Line s) param_sigs
           @ [B.Line (sprintf "%s ->" arg_type); B.Line "Yojson.Safe.t"]);
      ]

let gen_make_sig ((loc, (name, params, _), e) : A.type_def) : B.t =
  match e with
  | Record (_, fields, _) ->
      let fields =
        List.filter_map (function
          | `Field x -> Some x
          | `Inherit _ -> assert false)
          fields
      in
      let gen_param_sig (loc, (fname, kind, an), e) =
        match kind with
        | Required -> sprintf "%s:%s ->" fname (type_expr_str e)
        | Optional ->
            let inner =
              match e with
              | Option (_, ie, _) -> ie
              | _ -> e
            in
            sprintf "?%s:%s ->" fname (type_expr_str inner)
        | With_default -> sprintf "?%s:%s ->" fname (type_expr_str e)
      in
      let param_sigs = List.map gen_param_sig fields in
      let return_type = full_type_name name params in
      [
        B.Line
          (sprintf "val %s : %sunit -> %s"
             (make_name name)
             (String.concat " " param_sigs ^ " ")
             return_type);
      ]
  | _ -> []

let gen_io_sigs name : B.t =
  [
    B.Line (sprintf "val %s_of_string : string -> %s" name name);
    B.Line (sprintf "val string_of_%s : %s -> string" name name);
    B.Line (sprintf "val %s_of_channel : in_channel -> %s" name name);
    B.Line (sprintf "val %s_of_file : string -> %s" name name);
  ]

(* ============ Assemble the .ml file ============ *)

let make_ml ~atd_filename (items : A.module_body) : B.t =
  let defs = List.map (fun (Type x) -> x) items in
  let header =
    [
      B.Line (sprintf "(* Auto-generated from \"%s\". *)" atd_filename);
      B.Line "[@@@ocaml.warning \"-27-32-33-35-39\"]";
      B.Line "";
    ]
  in
  (* All type definitions *)
  let type_defs =
    match defs with
    | [] -> []
    | [def] -> gen_type_def def @ [B.Line ""]
    | first :: rest ->
        (* Use 'type ... and ...' for the whole block so mutual recursion works *)
        let first_lines = gen_type_def first in
        let rest_lines =
          List.concat_map (fun def ->
            (* Replace 'type' with 'and' for subsequent definitions *)
            match gen_type_def def with
            | B.Line s :: rest when String.length s >= 4
              && String.sub s 0 4 = "type" ->
                B.Line ("and" ^ String.sub s 4 (String.length s - 4)) :: rest
            | lines -> lines
          ) rest
        in
        first_lines @ rest_lines @ [B.Line ""]
  in
  (* Creation functions (no mutual recursion needed) *)
  let make_funs =
    List.concat_map (fun def ->
      let lines = gen_make_fun def in
      if lines = [] then [] else lines @ [B.Line ""]
    ) defs
  in
  (* of_yojson functions - one 'let rec ... and ...' block *)
  let of_yojson_funs =
    match defs with
    | [] -> []
    | defs ->
        let all =
          List.mapi (fun i def ->
            let lines = gen_of_yojson def in
            (* Replace 'let' with 'let rec' for first, 'and' for rest *)
            match lines with
            | B.Line s :: rest ->
                let keyword =
                  if i = 0 then
                    if String.length s >= 3 && String.sub s 0 3 = "let" then
                      "let rec" ^ String.sub s 3 (String.length s - 3)
                    else s
                  else
                    if String.length s >= 3 && String.sub s 0 3 = "let" then
                      "and" ^ String.sub s 3 (String.length s - 3)
                    else s
                in
                B.Line keyword :: rest
            | lines -> lines
          ) defs
        in
        List.concat all @ [B.Line ""]
  in
  (* yojson_of functions - one 'let rec ... and ...' block *)
  let yojson_of_funs =
    match defs with
    | [] -> []
    | defs ->
        let all =
          List.mapi (fun i def ->
            let lines = gen_yojson_of def in
            match lines with
            | B.Line s :: rest ->
                let keyword =
                  if i = 0 then
                    if String.length s >= 3 && String.sub s 0 3 = "let" then
                      "let rec" ^ String.sub s 3 (String.length s - 3)
                    else s
                  else
                    if String.length s >= 3 && String.sub s 0 3 = "let" then
                      "and" ^ String.sub s 3 (String.length s - 3)
                    else s
                in
                B.Line keyword :: rest
            | lines -> lines
          ) defs
        in
        List.concat all @ [B.Line ""]
  in
  (* Top-level I/O functions (only for non-parametrized types) *)
  let io_funs =
    List.concat_map (fun (_, (name, params, _), _) ->
      if params = [] then gen_io_funs name @ [B.Line ""]
      else []
    ) defs
  in
  header @ runtime_module @ [B.Line ""] @ type_defs @ make_funs @ of_yojson_funs @ yojson_of_funs @ io_funs

(* ============ Assemble the .mli file ============ *)

let make_mli ~atd_filename (items : A.module_body) : B.t =
  let defs = List.map (fun (Type x) -> x) items in
  let header =
    [
      B.Line (sprintf "(* Auto-generated from \"%s\". *)" atd_filename);
      B.Line "";
    ]
  in
  (* All type definitions (same as .ml) *)
  let type_defs =
    match defs with
    | [] -> []
    | [def] -> gen_type_def def @ [B.Line ""]
    | first :: rest ->
        let first_lines = gen_type_def first in
        let rest_lines =
          List.concat_map (fun def ->
            match gen_type_def def with
            | B.Line s :: rest when String.length s >= 4
              && String.sub s 0 4 = "type" ->
                B.Line ("and" ^ String.sub s 4 (String.length s - 4)) :: rest
            | lines -> lines
          ) rest
        in
        first_lines @ rest_lines @ [B.Line ""]
  in
  (* Signatures for each type's functions *)
  let sigs =
    List.concat_map (fun ((_, (name, params, _), _) as def) ->
      let make_sigs = gen_make_sig def in
      let of_sig = gen_of_yojson_sig def in
      let yojson_sig = gen_yojson_of_sig def in
      let io_sigs = if params = [] then gen_io_sigs name else [] in
      make_sigs @ of_sig @ yojson_sig @ io_sigs @ [B.Line ""]
    ) defs
  in
  header @ type_defs @ sigs

(* ============ Entry point ============ *)

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
  let _atd_head, atd_module = full_module in
  let ml_contents = make_ml ~atd_filename:src_name atd_module in
  let mli_contents = make_mli ~atd_filename:src_name atd_module in
  B.to_file ~indent:2 ml_path ml_contents;
  B.to_file ~indent:2 mli_path mli_contents
