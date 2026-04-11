(* Bridge between YAMLx and the ATD jsonlike AST.
   See Atd_yamlx.mli for documentation. *)

open Atd_jsonlike

(* ===== Location conversion ===== *)

(* YAMLx.pos uses 1-based line numbers and 0-based columns.
   Atd_jsonlike.Pos.t uses 0-based row and 0-based column. *)
let convert_pos (p : YAMLx.pos) : Pos.t = {
  Pos.row    = p.line - 1;  (* 1-based → 0-based *)
  Pos.column = p.column;    (* already 0-based *)
}

let convert_loc ?path (l : YAMLx.loc) : Loc.t = {
  Loc.start = convert_pos l.start_pos;
  Loc.end_  = convert_pos l.end_pos;
  Loc.path;
}

(* ===== Key conversion ===== *)

(* YAML map keys can be any value, but JSON object keys must be strings.
   We accept all scalar types and reject complex keys (sequences, maps). *)
let key_to_string (key : YAMLx.value) : string =
  match key with
  | YAMLx.String (_, s) -> s
  | YAMLx.Null _        -> "null"
  | YAMLx.Bool (_, b)   -> string_of_bool b
  | YAMLx.Int  (_, i)   -> Int64.to_string i
  | YAMLx.Float (_, f)  ->
      (* Use the JSON number representation: prefer an integer string when the
         value is whole, fall back to OCaml's default float formatting. *)
      let n = Atd_jsonlike.Number.of_float f in
      (match n.Number.int with
       | Some i -> string_of_int i
       | None   -> string_of_float f)
  | YAMLx.Seq _ ->
      invalid_arg
        "Atd_yamlx.of_yamlx_value: YAML sequence used as a map key; \
         only scalar keys (null, bool, int, float, string) are supported"
  | YAMLx.Map _ ->
      invalid_arg
        "Atd_yamlx.of_yamlx_value: YAML mapping used as a map key; \
         only scalar keys (null, bool, int, float, string) are supported"

(* ===== Value conversion ===== *)

(* Convert an int64 from YAMLx to Atd_jsonlike.Number.t.
   Tries to fit into a native int first (exact), then falls back to the
   decimal string representation so the literal field is preserved. *)
let number_of_int64 (i : int64) : Number.t =
  match Int64.to_int_opt i with
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

let rec of_yamlx_value ?path (v : YAMLx.value) : AST.t =
  let loc l = convert_loc ?path l in
  match v with
  | YAMLx.Null l ->
      AST.Null (loc l)

  | YAMLx.Bool (l, b) ->
      AST.Bool (loc l, b)

  | YAMLx.Int (l, i) ->
      AST.Number (loc l, number_of_int64 i)

  | YAMLx.Float (l, f) ->
      AST.Number (loc l, Number.of_float f)

  | YAMLx.String (l, s) ->
      AST.String (loc l, s)

  | YAMLx.Seq (l, items) ->
      AST.Array (loc l, List.map (of_yamlx_value ?path) items)

  | YAMLx.Map (l, pairs) ->
      (* Each pair is (pair_loc, key_value, value_value).
         pair_loc is the source range of the key (used as the key location
         in the jsonlike Object). *)
      let convert_pair (key_loc, key, value) =
        (convert_loc ?path key_loc, key_to_string key, of_yamlx_value ?path value)
      in
      AST.Object (loc l, List.map convert_pair pairs)
