(*
   Format comparison results in a human-readable form
*)

open Printf
open Types

let format_loc_text opt_loc =
  match opt_loc with
  | None -> ""
  | Some loc -> Atd.Ast.string_of_loc loc ^ "\n"

let format_incompatibility_text buf (x : incompatibility) =
  let is_certain =
    match x.kind with
    | Missing_field _ -> true
    | Default_required _ -> false
    | Incompatible_type _ -> true
    | Deleted_root_type _ -> false
    | Added_root_type _ -> false
  in
  let dir =
    match x.direction, is_certain with
    | Forward, true -> "Forward incompatibility"
    | Backward, true -> "Backward incompatibility"
    | Both, true -> "Incompatibility in both directions"
    | Forward, false -> "Possible forward incompatibility"
    | Backward, false -> "Possible backward incompatibility"
    | Both, false -> "Possible forward and backward incompatibility"
  in
  let msg =
    match x.kind, x.direction with
    | Missing_field {def_name; field_name}, Forward ->
        sprintf "\
A required field '%s' was added to the definition of type '%s'.
"
          field_name def_name
    | Missing_field {def_name; field_name}, Backward ->
        sprintf "\
A required field '%s' was removed from the definition of type '%s'.
"
          field_name def_name
    | _ -> "[TODO]"
  in
  bprintf buf "\
%s:
%s%s%s
"
    dir
    (format_loc_text x.location_old)
    (format_loc_text x.location_new)
    msg

let to_string res : string =
  let buf = Buffer.create 1000 in
  List.iter (format_incompatibility_text buf) res;
  Buffer.contents buf
