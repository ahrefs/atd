(*
   Format comparison results in a human-readable form
*)

open Printf
open Types

let format_loc_text opt_loc =
  match opt_loc with
  | None -> ""
  | Some loc -> Atd.Ast.string_of_loc loc ^ "\n"

let format_incompatibility_text buf (x : finding) =
  let is_certain =
    (* TODO: more clearly distinguish Warning from Error? *)
    match x.kind with
    | Missing_field _ -> true
    | Missing_variant _ -> true
    | Missing_variant_argument _ -> true
    | Default_required -> false
    | Incompatible_type -> true
    | Deleted_root_type -> false
    | Added_root_type -> false
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
(*
   TODO: this should populate the 'description' field earlier:

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
*)
  bprintf buf "\
%s:
%s%s%s
"
    dir
    (format_loc_text x.location_old)
    (format_loc_text x.location_new)
    x.description

let to_string res : string =
  let buf = Buffer.create 1000 in
  List.iter (fun x ->
    format_incompatibility_text buf x;
    Buffer.add_char buf '\n'
  ) res;
  Buffer.contents buf
