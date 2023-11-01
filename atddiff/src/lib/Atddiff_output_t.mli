(* Auto-generated from "Atddiff_output.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type variant_info = { variant_name: string }

type position = { path: string; line: int; column: int }

type location = { start: position; end_ (*atd end *): position }

type field_info = { field_name: string }

type incompatibility_kind = 
    Missing_field of field_info
  | Missing_variant of variant_info
  | Missing_variant_argument of variant_info
  | Default_required of field_info
  | Incompatible_type
  | Deleted_type
  | Added_type


type direction =  Backward | Forward | Both 

type finding = {
  hash: string;
  direction: direction;
  kind: incompatibility_kind;
  location_old: location option;
  location_new: location option;
  description: string;
  affected_types: string list
}

type result = { findings: finding list }
