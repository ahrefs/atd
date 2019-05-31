(*
  Mapping from ATD to WWW-form data
*)

type www_adapter = {
  ocaml_adapter : string option;
}

let no_adapter = {
  ocaml_adapter = None;
}

type www_float =
  | Float of int option (* max decimal places *)
  | Int

type www_list = Array | Object

type www_variant = { www_cons : string }

type www_field = {
  www_fname  : string;           (* <www name=...> *)
  www_unwrapped : bool;
}

type www_record = {
  www_record_adapter : www_adapter;
}

type www_sum = {
  www_sum_adapter : www_adapter;
  www_open_enum : bool;
  www_lowercase_tags : bool;
}

let section = "www"

let sections = [ section ]

(*
   Note that www adapters are supported only by records and sums
   at this time.
   TODO: Support www adapters for all kinds of nodes rather than just
   sums and records, preferably without major code duplication.
   Maybe this can be achieved by turning www_repr
   into (www_repr * www_adapter).
*)
type www_repr =
  | Bool
  | Cell
  | Def
  | External
  | Field of www_field
  | Float of www_float
  | Int
  | List of www_list
  | Nullable
  | Option
  | Record of www_record
  | String
  | Sum of www_sum
  | Tuple
  | Unit
  | Variant of www_variant
  | Wrap (* should we add support for Base64 encoding of binary data? *)

let www_float_of_string s : [ `Float | `Int ] option =
  match s with
      "float" -> Some `Float
    | "int" -> Some `Int
    | _ -> None

let www_precision_of_string s =
  try Some (int_of_string s)
  with _ -> None

let get_www_precision an =
  Atd.Annot.get_opt_field
    ~parse:www_precision_of_string
    ~sections
    ~field:"precision"
    an

let get_www_float an : www_float =
  match
    Atd.Annot.get_field
      ~parse:www_float_of_string
      ~default:`Float
      ~sections
      ~field:"repr"
      an
  with
      `Float -> Float (get_www_precision an)
    | `Int -> Int

let www_list_of_string s : www_list option =
  match s with
  | "array" -> Some Array
  | "object" -> Some Object
  | _ -> (* error *) None

(*
   <www adapter.ocaml="Foo.Bar">
   --> { ocaml_adapter = Some "Foo.Bar"; }
*)
let get_www_adapter an =
  let ocaml_adapter =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections
      ~field:"adapter.ocaml"
      an
  in
  { ocaml_adapter }

let get_www_open_enum an =
  Atd.Annot.get_flag ~sections ~field:"open_enum" an

let get_www_lowercase_tags an =
  Atd.Annot.get_flag ~sections ~field:"lowercase_tags" an

let get_www_sum an = {
  www_sum_adapter = get_www_adapter an;
  www_open_enum = get_www_open_enum an;
  www_lowercase_tags = get_www_lowercase_tags an;
}

let get_www_list an =
  Atd.Annot.get_field
    ~parse:www_list_of_string
    ~default:Array
    ~sections
    ~field:"repr"
    an

let get_www_cons default an =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections
    ~field:"name"
    an

let get_www_fname default an =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections
    ~field:"name"
    an

let get_www_record an =
  {
    www_record_adapter = get_www_adapter an;
  }

let tests = [
]
