(*
   JSON-like AST implementation
*)

open Printf

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

let rec equal a b =
  match a, b with
  | Null _, Null _ -> true
  | Bool (_, x), Bool (_, y) -> x = y
  | Number (_, x), Number (_, y) ->
      (* Compare by the most precise representation available *)
      (match x.Number.literal, y.Number.literal with
       | Some s1, Some s2 -> s1 = s2
       | _ ->
           match x.Number.int, y.Number.int with
           | Some i1, Some i2 -> i1 = i2
           | _ ->
               match x.Number.float, y.Number.float with
               | Some f1, Some f2 -> f1 = f2
               | _ -> false)
  | String (_, x), String (_, y) -> x = y
  | Array (_, xs), Array (_, ys) -> List.equal equal xs ys
  | Object (_, xs), Object (_, ys) ->
      List.equal (fun (_, k1, v1) (_, k2, v2) -> k1 = k2 && equal v1 v2) xs ys
  | _ -> false

let rec compare a b =
  match a, b with
  | Null _, Null _ -> 0
  | Null _, _ -> -1 | _, Null _ -> 1
  | Bool (_, x), Bool (_, y) -> Bool.compare x y
  | Bool _, _ -> -1 | _, Bool _ -> 1
  | Number (_, x), Number (_, y) ->
      (match x.Number.literal, y.Number.literal with
       | Some s1, Some s2 -> String.compare s1 s2
       | _ ->
           match x.Number.float, y.Number.float with
           | Some f1, Some f2 -> Float.compare f1 f2
           | _ -> 0)
  | Number _, _ -> -1 | _, Number _ -> 1
  | String (_, x), String (_, y) -> String.compare x y
  | String _, _ -> -1 | _, String _ -> 1
  | Array (_, xs), Array (_, ys) -> List.compare compare xs ys
  | Array _, _ -> -1 | _, Array _ -> 1
  | Object (_, xs), Object (_, ys) ->
      List.compare
        (fun (_, k1, v1) (_, k2, v2) ->
           let c = String.compare k1 k2 in
           if c <> 0 then c else compare v1 v2)
        xs ys

(* Produce diff-friendly output. It's not strict JSON mostly because we
   don't want to depend on a JSON library for this. Maybe we could
   use the YAMLx pretty-printer but then it will look even less like JSON. *)
let show (x : t) : string =
  let buf = Buffer.create 100 in
  let line prefix ?(suffix = "") str =
    bprintf buf "%s%s%s\n" prefix str suffix
  in
  let rec show prefix = function
    | Null _ -> line prefix "null"
    | Bool (_, b) -> line prefix (string_of_bool b)
    | Number (_, n) ->
        (match n.Number.literal with
         | Some s -> s
         | None ->
             match n.Number.int with
             | Some i -> string_of_int i
             | None ->
                 match n.Number.float with
                 | Some f -> string_of_float f
                 | None -> "<number>")
        |> line prefix
    | String (_, s) -> line prefix (sprintf "%S" s)
    | Array (_, []) ->
        line prefix "[]"
    | Array (_, items) ->
        line prefix "[";
        List.iter (show ("  " ^ prefix)) items;
        line prefix "]"
    | Object (_, []) ->
        line prefix "{}"
    | Object (_, fields) ->
        line prefix "{";
        List.iter (show_field ("  " ^ prefix)) fields;
        line prefix "}"
  and show_field prefix (_, k, v) =
    line prefix ~suffix:":" (sprintf "%S" k);
    show ("  " ^ prefix) v
  in
  show "" x;
  Buffer.contents buf

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
