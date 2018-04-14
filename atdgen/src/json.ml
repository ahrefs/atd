(*
  Mapping from ATD to JSON
*)

open Printf

type json_float =
  | Float of int option (* max decimal places *)
  | Int

type json_adapter = string list

type json_list = Array | Object

type json_variant = { json_cons : string option }

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
  | Sum of json_adapter option
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

let parse_adapter_name =
  let component = "[a-z][a-z0-9_]*" in
  let component_re = Str.regexp (sprintf "^%s$" component) in
  let check_component s =
    Str.string_match component_re s 0 && Str.match_end () = String.length s
  in
  fun s ->
    let components = String.split_on_char '.' s in
    if List.for_all check_component components then
      Some components
    else
      None

let test_parse_adapter_name () =
  let ok s components =
    parse_adapter_name s = Some components
  in
  let not_ok s =
    parse_adapter_name s = None
  in
  assert (ok "a" ["a"]);
  assert (ok "a.b" ["a"; "b"]);
  assert (ok "ab.cd" ["ab"; "cd"]);
  assert (ok "ab.cd.e_5_" ["ab"; "cd"; "e_5_"]);
  assert (not_ok "");
  assert (not_ok "A");
  assert (not_ok "a.");
  assert (not_ok "a\n");
  assert (not_ok "\na");
  assert (not_ok "a._b");
  assert (not_ok "a..b");
  true

(* <json adapter="foo.bar"> -> Some ["foo"; "bar"] *)
let json_adapter_of_string s : json_adapter option option =
  Some (parse_adapter_name s)

let json_list_of_string s : json_list option =
  match s with
  | "array" -> Some Array
  | "object" -> Some Object
  | _ -> (* error *) None

let get_json_adapter an =
  Atd.Annot.get_field json_adapter_of_string None ["json"] "adapter" an

let get_json_sum = get_json_adapter

let get_json_list an =
  Atd.Annot.get_field json_list_of_string Array ["json"] "repr" an

let get_json_cons default an =
  Atd.Annot.get_field (fun s -> Some s) default ["json"] "name" an

let get_json_fname default an =
  Atd.Annot.get_field (fun s -> Some s) default ["json"] "name" an

let get_json_tag_field an =
  Atd.Annot.get_field (fun s -> Some (Some s)) None ["json"] "tag_field" an

let get_json_untyped an =
  Atd.Annot.get_flag ["json"] "untyped" an

let get_json_keep_nulls an =
  Atd.Annot.get_flag ["json"] "keep_nulls" an

let get_json_record an =
  {
    json_keep_nulls = get_json_keep_nulls an;
  }

let tests = [
  "parse adapter name", test_parse_adapter_name;
]
