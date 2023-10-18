(*
   Format comparison results in a human-readable form
*)

open Printf
module T = Atddiff_output_t

let format_loc_text opt_loc =
  match opt_loc with
  | None -> ""
  | Some loc -> Loc.to_string loc ^ "\n"

let format_incompatibility_text buf (x : T.full_finding) =
  let finding = x.finding in
  let is_certain =
    (* TODO: more clearly distinguish Warning from Error? *)
    match finding.kind with
    | Missing_field _ -> true
    | Missing_variant _ -> true
    | Missing_variant_argument _ -> true
    | Default_required _ -> false
    | Incompatible_type -> true
    | Deleted_type -> false
    | Added_type -> false
  in
  let dir =
    match finding.direction, is_certain with
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
The following types are affected:%s
"
    dir
    (format_loc_text finding.location_old)
    (format_loc_text finding.location_new)
    finding.description
    (x.affected_types
     |> List.map (fun name -> "\n  " ^ name)
     |> String.concat "")

let to_string (res : T.result) : string =
  let buf = Buffer.create 1000 in
  List.iter (fun x ->
    format_incompatibility_text buf x;
    Buffer.add_char buf '\n'
  ) res.findings;
  Buffer.contents buf
