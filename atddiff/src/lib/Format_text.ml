(*
   Format comparison results in a human-readable form
*)

open Printf
open Types

let format_loc_text opt_loc =
  match opt_loc with
  | None -> ""
  | Some loc -> Atd.Ast.string_of_loc loc ^ "\n"

let format_incompatibility_text buf ((x : finding), affected_root_types) =
  let is_certain =
    (* TODO: more clearly distinguish Warning from Error? *)
    match x.kind with
    | Missing_field _ -> true
    | Missing_variant _ -> true
    | Missing_variant_argument _ -> true
    | Default_required _ -> false
    | Incompatible_type -> true
    | Deleted_root_type -> false
    | Added_root_type -> false
    | Parametrized_root_type -> false
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
  bprintf buf "\
%s:
%s%s%s
The following root types are affected:%s
"
    dir
    (format_loc_text x.location_old)
    (format_loc_text x.location_new)
    x.description
    (affected_root_types
     |> List.map (fun name -> "\n  " ^ name)
     |> String.concat "")

let to_string (res : result) : string =
  let buf = Buffer.create 1000 in
  List.iter (fun x ->
    format_incompatibility_text buf x;
    Buffer.add_char buf '\n'
  ) res;
  Buffer.contents buf
