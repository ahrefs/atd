(*
  Mapping from ATD to JSON
*)

type json_float =
  | Float of int option (* max decimal places *)
  | Int

type json_adapter = {
  ocaml_adapter : string option;
  java_adapter : string option;
}

let no_adapter = {
  ocaml_adapter = None;
  java_adapter = None;
}

type json_list = Array | Object

type json_variant = { json_cons : string }

type json_field = {
  json_fname  : string;           (* <json name=...> *)
  json_tag_field : string option; (* <json tag_field=...> *)
  json_unwrapped : bool;
}

type json_record = {
  json_keep_nulls : bool; (* { ... } <json keep_nulls> *)
}

type json_repr =
  | Bool
  | Cell
  | Def
  | External
  | Field of json_field
  | Float of json_float
  | Int
  | List of json_list
  | Nullable
  | Option
  | Record of json_record
  | String
  | Sum of json_adapter
  | Tuple
  | Unit
  | Variant of json_variant
  | Wrap (* should we add support for Base64 encoding of binary data? *)

let json_float_of_string s : [ `Float | `Int ] option =
  match s with
      "float" -> Some `Float
    | "int" -> Some `Int
    | _ -> None

let json_precision_of_string s =
  try Some (Some (int_of_string s))
  with _ -> None

let get_json_precision an =
  Atd.Annot.get_field
    json_precision_of_string None ["json"] "precision" an

let get_json_float an : json_float =
  match
    Atd.Annot.get_field json_float_of_string `Float ["json"] "repr" an
  with
      `Float -> Float (get_json_precision an)
    | `Int -> Int

let json_list_of_string s : json_list option =
  match s with
  | "array" -> Some Array
  | "object" -> Some Object
  | _ -> (* error *) None

(*
   <json adapter.ocaml="Foo.Bar">
   --> { ocaml_adapter = Some "Foo.Bar";
         java_adapter = None; }
*)
let get_json_adapter an =
  let ocaml_adapter =
    Atd.Annot.get_field (fun s -> Some (Some s))
      None ["json"] "adapter.ocaml" an
  in
  let java_adapter =
    Atd.Annot.get_field (fun s -> Some (Some s))
      None ["json"] "adapter.java" an
  in
  { ocaml_adapter;
    java_adapter }

let get_json_sum = get_json_adapter

let get_json_list an =
  Atd.Annot.get_field json_list_of_string Array ["json"] "repr" an

let get_json_cons default an =
  Atd.Annot.get_field (fun s -> Some s) default ["json"] "name" an

let get_json_fname default an =
  Atd.Annot.get_field (fun s -> Some s) default ["json"] "name" an

let get_json_tag_field an =
  Atd.Annot.get_field (fun s -> Some (Some s)) None ["json"] "tag_field" an

let get_json_keep_nulls an =
  Atd.Annot.get_flag ["json"] "keep_nulls" an

let get_json_record an =
  {
    json_keep_nulls = get_json_keep_nulls an;
  }

let tests = [
]
