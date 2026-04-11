(* Bridge between YAMLx and the ATD jsonlike AST.
   See Atd_yamlx.mli for documentation. *)

open Atd_jsonlike

(* ===== Location conversion ===== *)

(* YAMLx.pos uses 1-based line numbers and 0-based columns.
   Atd_jsonlike.Pos.t uses 0-based row and 0-based column. *)
let convert_pos (p : YAMLx.pos) : Pos.t = {
  row    = p.line - 1;  (* 1-based → 0-based *)
  column = p.column;    (* already 0-based *)
}

let convert_loc ?path (l : YAMLx.loc) : Loc.t = {
  start = convert_pos l.start_pos;
  end_  = convert_pos l.end_pos;
  path;
}

(* ===== Key conversion ===== *)

(* YAML map keys must be strings for JSON object compatibility.
   Non-string keys could be converted to strings in multiple ways and
   the choice would be arbitrary, so we require users to pre-process
   their data if they need non-string keys. *)
let key_to_string ?path (key : YAMLx.value) : string =
  let open YAMLx in
  match key with
  | String (_, s) -> s
  | Null loc | Bool (loc, _) | Int (loc, _) | Float (loc, _)
  | Seq (loc, _) | Map (loc, _) ->
      let loc_str = YAMLx.default_format_loc ?file:path loc in
      invalid_arg
        (loc_str ^ "map key must be a string; \
         pre-process the YAML document to convert non-string keys if needed")

(* ===== Value conversion ===== *)

(* Convert an int64 from YAMLx to Atd_jsonlike.Number.t.
   Tries to fit into a native int first (exact), then falls back to the
   decimal string representation so the literal field is preserved. *)
let number_of_int64 (i : int64) : Number.t =
  (* Check whether i fits in a native int via a roundtrip. *)
  let to_int_opt i =
    let n = Int64.to_int i in
    if Int64.of_int n = i then Some n else None
  in
  match to_int_opt i with
  | Some n -> Number.of_int n
  | None   ->
      (* The value doesn't fit in a native OCaml int (possible on 32-bit
         platforms for large YAML integers).  Use the decimal string; this
         always produces a valid JSON number literal. *)
      match Number.of_string_opt (Int64.to_string i) with
      | Some n -> n
      | None   ->
          (* Int64.to_string is always a valid decimal — this cannot happen. *)
          assert false

let rec of_yamlx_value_exn ?path (v : YAMLx.value) : AST.t =
  let open YAMLx in
  let loc l = convert_loc ?path l in
  match v with
  | Null l -> Null (loc l)
  | Bool (l, b) -> Bool (loc l, b)
  | Int (l, i) -> Number (loc l, number_of_int64 i)
  | Float (l, f) -> Number (loc l, Number.of_float f)
  | String (l, s) -> String (loc l, s)
  | Seq (l, items) -> Array (loc l, List.map (of_yamlx_value_exn ?path) items)
  | Map (l, pairs) ->
      (* Each pair is (pair_loc, key_value, value_value).
         pair_loc is the source range of the key (used as the key location
         in the jsonlike Object). *)
      let convert_pair (key_loc, key, value) =
        (convert_loc ?path key_loc,
         key_to_string ?path key,
         of_yamlx_value_exn ?path value)
      in
      Object (loc l, List.map convert_pair pairs)

let of_yamlx_value ?path v =
  match of_yamlx_value_exn ?path v with
  | result              -> Ok result
  | exception Invalid_argument msg -> Error ("invalid argument: " ^ msg)
