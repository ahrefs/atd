(*
   Read ATD annotations relating to OCaml.
*)

open Printf
module R = Ocaml_repr

let path_of_target (target : R.target) =
  match target with
    | Default -> [ "ocaml" ]
    | Biniou -> [ "ocaml_biniou"; "ocaml" ]
    | Json -> [ "ocaml_json"; "ocaml" ]
    | Bucklescript -> ["ocaml_bs"; "ocaml"]
    | Validate -> [ "ocaml_validate"; "ocaml" ]

(*
   This must hold all the valid annotations of the form
   '<ocaml ...>' or '<ocaml_TARGET>' (see above for the target names).
*)
let annot_schema_ocaml : Atd.Annot.schema_section =
  {
    section = "ocaml";
    fields = [
      Type_def, "attr";
      Type_def, "from";
      Type_def, "module";
      Type_def, "predef";
      Type_def, "t";
      Type_expr, "field_prefix";
      Type_expr, "module";
      Type_expr, "repr";
      Type_expr, "t";
      Type_expr, "unwrap";
      Type_expr, "valid";
      Type_expr, "validator";
      Type_expr, "wrap";
      Variant, "name";
      Cell, "default";
      Field, "default";
      Field, "mutable";
      Field, "name";
      Field, "repr";
    ]
  }

let annot_schema_of_target (target : R.target) : Atd.Annot.schema =
  let section_names = path_of_target target in
  let ocaml_sections =
    List.map
      (fun section -> { annot_schema_ocaml with section }) section_names
  in
  let other_section =
    match target with
    | Default -> []
    | Biniou -> Biniou.annot_schema_biniou
    | Json -> Atd.Json.annot_schema_json
    | Bucklescript -> Atd.Json.annot_schema_json
    | Validate -> []
  in
  ocaml_sections @ other_section

let get_ocaml_int target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:R.ocaml_int_of_string
    ~default:Int
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_type_path (env : R.env) loc (atd_name : Atd.Ast.type_name) an =
  let x =
    match atd_name with
    | TN ["unit"] -> `Unit
    | TN ["bool"] -> `Bool
    | TN ["int"] -> `Int (get_ocaml_int env.target an)
    | TN ["float"] -> `Float
    | TN ["string"] -> `String
    | TN ["abstract"] -> `Abstract
    | TN _ ->
        let import, base_name =
          Atd.Imports.resolve env.imports loc atd_name
        in
        match import with
        | None -> `Name base_name
        | Some x -> `Name (R.ocaml_type_module_name x.name)
  in
  match x with
  | `Unit -> "unit"
  | `Bool -> "bool"
  | `Int x -> R.string_of_ocaml_int x
  | `Float -> "float"
  | `String -> "string"
  | `Abstract -> "Yojson.Safe.t"
  | `Name s -> s

let get_ocaml_sum target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:R.ocaml_sum_of_string
    ~default:Poly
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_field_prefix target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:""
    ~sections:path
    ~field:"field_prefix"
    an

let get_ocaml_record target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:R.ocaml_record_of_string
    ~default:Record
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_list target an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:R.ocaml_list_of_string
    ~default:List
    ~sections:path
    ~field:"repr"
    an

let get_ocaml_wrap ~type_param target loc an : R.atd_ocaml_wrap option =
  let path = path_of_target target in
  let module_ =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:path
      ~field:"module"
      an
  in
  let default field =
    Option.map (fun s ->
      sprintf "%s.%s" s field) module_
  in
  let default_t field =
    Option.map (fun s ->
      let type_param =
        match List.map (sprintf "'%s") type_param with
        | [] -> ""
        | x::[] -> sprintf "%s " x
        | param -> sprintf "(%s) " (String.concat ", " type_param) in
      sprintf "%s%s.%s" type_param s field) module_
  in
  let t =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default_t "t")
      ~sections:path
      ~field:"t"
      an
  in
  let wrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "wrap")
      ~sections:path
      ~field:"wrap"
      an
  in
  let unwrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "unwrap")
      ~sections:path
      ~field:"unwrap"
      an
  in
  match t, wrap, unwrap with
      None, None, None -> None
    | Some t, Some wrap, Some unwrap ->
        Some { ocaml_wrap_t = t; ocaml_wrap = wrap; ocaml_unwrap = unwrap }
    | _ ->
        Error.error loc "Incomplete annotation. Missing t, wrap or unwrap"

let get_ocaml_cons target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:path
    ~field:"name"
    an

let get_ocaml_fname target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default
    ~sections:path
    ~field:"name"
    an

let get_ocaml_default target an =
  let path = path_of_target target in
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:path
    ~field:"default"
    an

let get_ocaml_mutable target an =
  let path = path_of_target target in
  Atd.Annot.get_flag
    ~sections:path
    ~field:"mutable"
    an

let get_ocaml_predef target an =
  let path = path_of_target target in
  Atd.Annot.get_flag
    ~sections:path
    ~field:"predef"
    an

let get_ocaml_module target an =
  let path = path_of_target target in
  let o =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:path
      ~field:"module"
      an
  in
  match o with
    Some s -> Some (s, s)
  | None ->
      Atd.Annot.get_opt_field
        ~parse:(fun s -> Some s)
        ~sections:path
        ~field:"from" an
      |> Option.map (fun s ->
        let type_module = R.ocaml_type_module_name s in
        let main_module = R.ocaml_module_name target s in
        (type_module, main_module))

let get_ocaml_t target default an =
  let path = path_of_target target in
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default
    ~sections:path
    ~field:"t"
    an

let get_ocaml_module_and_t target default_name an =
  get_ocaml_module target an
  |> Option.map (fun (type_module, main_module) ->
  (type_module, main_module, get_ocaml_t target default_name an))

let get_type_attrs an =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["ocaml"]
    ~field:"attr"
    an
