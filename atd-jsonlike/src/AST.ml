type t =
  | Null of Loc.t
  | Bool of Loc.t * bool
  | Number of Loc.t * Number.t
  | String of Loc.t * string
  | Array of Loc.t * t list
  | Object of Loc.t * (Loc.t * string * t) list

let get_loc = function
  | Null loc
  | Bool (loc, _)
  | Number (loc, _)
  | String (loc, _)
  | Array (loc, _)
  | Object (loc, _) -> loc

let loc_msg node =
  let loc = get_loc node in
  let { Loc.start; end_; path } = loc in
  let path_prefix =
    match path with
    | None -> ""
    | Some p -> Printf.sprintf "File %S, " p
  in
  if start.Pos.row = end_.Pos.row then
    Printf.sprintf "%sline %d, characters %d-%d:\n"
      path_prefix (start.Pos.row + 1) start.Pos.column end_.Pos.column
  else
    Printf.sprintf "%slines %d-%d, characters %d-%d:\n"
      path_prefix (start.Pos.row + 1) (end_.Pos.row + 1)
      start.Pos.column end_.Pos.column
