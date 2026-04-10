(* Support level for one feature in one target language. *)
type support =
  | Yes      (* supported *)
  | Planned  (* not yet, but could/should be *)
  | No       (* not supported and not planned *)

(* All features for one target language. Adding a new language means adding
   a new entry to the [languages] list below, starting from [all_yes] and
   overriding the features that are not fully supported. *)
type lang_support = {
  basic_types:        support;
  abstract:           support;
  list_:              support;
  option_:            support;
  nullable:           support;
  wrap:               support;
  records:            support;
  sum_types:          support;
  tuples:             support;
  parametric:         support;
  aliases:            support;
  optional_fields:    support;
  default_fields:     support;
  doc_comments:       support;
  json_field_names:   support;
  json_variant_names: support;
  json_repr_object:   support;
  sum_repr_object:    support;
  json_adapter:       support;
  imports:              support;
  binary_serialization: support;
  open_enums:           support;
}

(* A feature has a display name, a short description, and an accessor into
   [lang_support]. The accessor is what lets the printer iterate over features
   without a big match expression per language. *)
type feature = {
  name:        string;
  description: string;
  get:         lang_support -> support;
}

(* Row order in the output. Adding a new feature means adding a field to
   [lang_support], a [features] entry here, and updating each language
   below. *)
let features : feature list = [
  { name = "Basic types";          description = "unit, bool, int, float, string";              get = fun s -> s.basic_types };
  { name = "Abstract type";        description = "Any JSON value (abstract keyword)";           get = fun s -> s.abstract };
  { name = "List / array";         description = "The list type constructor";                   get = fun s -> s.list_ };
  { name = "Option type";          description = "ATD-style \"None\" / [\"Some\", x] encoding"; get = fun s -> s.option_ };
  { name = "Nullable";             description = "JSON null <-> None, other value <-> Some x";  get = fun s -> s.nullable };
  { name = "Wrap type";            description = "Custom type wrappers ('a wrap)";              get = fun s -> s.wrap };
  { name = "Records";              description = "Record types with named fields";              get = fun s -> s.records };
  { name = "Sum types";            description = "Tagged unions / variants";                    get = fun s -> s.sum_types };
  { name = "Tuples";               description = "Fixed-arity product types";                   get = fun s -> s.tuples };
  { name = "Parametric types";     description = "Generic / parameterized type definitions";    get = fun s -> s.parametric };
  { name = "Type aliases";         description = "Simple aliases (type t = int list)";          get = fun s -> s.aliases };
  { name = "Optional fields";      description = "?field — absent JSON key <-> None";           get = fun s -> s.optional_fields };
  { name = "Default-value fields"; description = "~field — absent JSON key uses a default";    get = fun s -> s.default_fields };
  { name = "Doc comments";         description = "<doc text=\"...\"> -> language docstrings";   get = fun s -> s.doc_comments };
  { name = "JSON field names";     description = "<json name=\"...\"> on record fields";        get = fun s -> s.json_field_names };
  { name = "JSON variant names";   description = "<json name=\"...\"> on sum type constructors"; get = fun s -> s.json_variant_names };
  { name = "Assoc as JSON object"; description = "(string * v) list <json repr=\"object\">";          get = fun s -> s.json_repr_object };
  { name = "Sum as JSON object";   description = "sum type <json repr=\"object\">: {\"Cons\": payload}"; get = fun s -> s.sum_repr_object };
  { name = "JSON adapter";         description = "Custom pre/post-processing hooks";                    get = fun s -> s.json_adapter };
  { name = "Cross-file imports";      description = "from module import type1, type2";                                                        get = fun s -> s.imports };
  { name = "Binary serialization";    description = "Biniou format: faster than JSON, field/constructor names encoded as low-collision hashes"; get = fun s -> s.binary_serialization };
  { name = "Open enumerations";       description = "Unknown variants round-tripped as-is";                                                     get = fun s -> s.open_enums };
]

(* Baseline: every feature supported. Each language starts from this and
   overrides only the exceptions. *)
let all_yes = {
  basic_types        = Yes;
  abstract           = Yes;
  list_              = Yes;
  option_            = Yes;
  nullable           = Yes;
  wrap               = Yes;
  records            = Yes;
  sum_types          = Yes;
  tuples             = Yes;
  parametric         = Yes;
  aliases            = Yes;
  optional_fields    = Yes;
  default_fields     = Yes;
  doc_comments       = Yes;
  json_field_names   = Yes;
  json_variant_names = Yes;
  json_repr_object   = Yes;
  sum_repr_object    = Yes;
  json_adapter       = Yes;
  imports              = Yes;
  binary_serialization = Yes;
  open_enums           = Yes;
}

(* The data. Each entry is (display name, support record). Column order in
   the output matches the order here. *)
let languages : (string * lang_support) list = [
  "atdml (OCaml)", { all_yes with
    open_enums = Planned;
    binary_serialization = No;
  };
  "atdgen (OCaml)", { all_yes with
    sum_repr_object      = Planned;
    imports              = No;
    binary_serialization = Yes;
  };
  "atdpy (Python)", { all_yes with
    wrap            = Planned;
    sum_repr_object = Planned;
    json_adapter    = Planned;
    open_enums      = Planned;
    binary_serialization = No;
  };
  "atdts (TypeScript)", { all_yes with
    sum_repr_object = Planned;
    json_adapter    = Planned;
    open_enums      = Planned;
    binary_serialization = No;
  };
  "atdj (Java)", { all_yes with
    wrap             = Planned;
    json_repr_object = Planned;
    json_adapter     = Planned;
    imports          = Planned;
    open_enums       = Planned;
    binary_serialization = No;
  };
  "atds (Scala)", { all_yes with
    wrap             = Planned;
    json_repr_object = Planned;
    sum_repr_object  = Planned;
    json_adapter     = Planned;
    imports          = Planned;
    open_enums       = Planned;
    binary_serialization = No;
  };
  "atdd (D)", { all_yes with
    doc_comments     = Planned;
    json_repr_object = Planned;
    sum_repr_object  = Planned;
    json_adapter     = Planned;
    imports          = Planned;
    open_enums       = Planned;
    binary_serialization = No;
  };
  "atdcpp (C++)", { all_yes with
    doc_comments     = Planned;
    json_repr_object = Planned;
    sum_repr_object  = Planned;
    json_adapter     = Planned;
    imports          = Planned;
    open_enums       = Planned;
    binary_serialization = No;
  };
]

let label_of = function
  | Yes     -> "Supported"
  | Planned -> "Not yet"
  | No      -> "Not supported"

(* Output: reStructuredText (RST) consumed by Sphinx to produce the
   doc/support-matrix page included in the ATD documentation.
   Each feature becomes an RST subsection (~~ underline). Under each
   subsection, languages are grouped by support level, with empty groups
   omitted, producing lines of the form:
     **Supported:** atdml (OCaml), atdgen (OCaml), ...
   Run `make` in this directory to regenerate doc/support-matrix.rst. *)
let print () =
  print_string ".. Generated by internal/support_matrix.ml. Do not edit directly.\n";
  print_string "   Run 'make' in internal/ to regenerate.\n";
  print_char '\n';
  print_string "Feature Support Matrix\n";
  print_string "======================\n";
  print_char '\n';
  print_string "For each ATD feature, target languages are grouped by support level.\n";
  print_char '\n';
  List.iter (fun feat ->
    let n = String.length feat.name in
    Printf.printf "%s\n%s\n\n" feat.name (String.make n '~');
    Printf.printf "%s\n\n" feat.description;
    List.iter (fun level ->
      let langs =
        List.filter_map
          (fun (lang, ls) -> if feat.get ls = level then Some lang else None)
          languages
      in
      if langs <> [] then
        Printf.printf "**%s:** %s\n\n" (label_of level) (String.concat ", " langs)
    ) [Yes; Planned; No]
  ) features

let () = print ()
