(*
   Convert (tediously) the atddiff findings to JSON.
*)

open Atddiff_output_t
type json = Yojson.Safe.t

let string x : json = `String x

let option f opt : json =
  match opt with
  | Some x -> f x
  | None -> `Null

let remove_null_fields xs : (string * json) list =
  List.filter (function (_, `Null) -> false | _ -> true) xs

let fmt_direction (x : direction) : json =
  match x with
  | Backward -> `String "Backward"
  | Forward -> `String "Forward"
  | Both -> `String "Both"

let fmt_field_info { field_name } : json =
  `Assoc [ "field_name", `String field_name ]

let fmt_variant_info { variant_name } : json =
  `Assoc [ "variant_name", `String variant_name ]

let fmt_kind (x : incompatibility_kind) : json =
  match x with
  | Missing_field x -> `List [`String "Missing_field"; fmt_field_info x]
  | Missing_variant x -> `List [`String "Missing_variant"; fmt_variant_info x]
  | Missing_variant_argument x ->
      `List [`String "Missing_variant_argument"; fmt_variant_info x]
  | Default_required x -> `List [`String "Default_required"; fmt_field_info x]
  | Incompatible_type -> `String "Incompatible_type"
  | Deleted_type -> `String "Deleted_type"
  | Added_type -> `String "Added_type"

let fmt_position (x : position) : json =
  `Assoc [
    "path", `String x.path;
    "line", `Int x.line;
    "column", `Int x.column;
  ]

let fmt_location (x : location) : json =
  `Assoc [
    "start", fmt_position x.start;
    "end", fmt_position x.end_;
  ]

let fmt_finding (x : finding) : json =
  `Assoc (remove_null_fields [
    "direction", fmt_direction x.direction;
    "kind", fmt_kind x.kind;
    "location_old", option fmt_location x.location_old;
    "location_new", option fmt_location x.location_new;
    "description", `String x.description;
  ])

let fmt_full_finding (x : full_finding) : json =
  `Assoc [
    "finding", fmt_finding x.finding;
    "affected_types", `List (List.map string x.affected_types);
  ]

let to_yojson (x : result) : json =
  `Assoc [
    "findings", `List (List.map fmt_full_finding x.findings)
  ]

let to_string x =
  x
  |> to_yojson
  |> Yojson.Safe.to_string
  |> Yojson.Safe.prettify
