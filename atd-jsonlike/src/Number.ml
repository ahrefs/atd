type t = {
  int: int option;
  float: float option;
  literal: string option;
}

let of_int n =
  { int = Some n; float = Some (Float.of_int n); literal = Some (string_of_int n) }

let of_float f =
  let int_opt =
    if Float.is_integer f then
      (try
        let n = int_of_float f in
        if Float.of_int n = f then Some n else None
      with _ -> None)
    else None
  in
  { int = int_opt; float = Some f; literal = None }

(* JSON grammar: minus? ( 0 | [1-9][0-9]* ) ( '.' [0-9]+ )? ( [eE] [+-]? [0-9]+ )? *)
let json_number_re =
  let open Re in
  compile (whole_string (seq [
    opt (char '-');
    alt [char '0'; seq [rg '1' '9'; rep digit]];
    opt (seq [char '.'; rep1 digit]);
    opt (seq [set "eE"; opt (set "+-"); rep1 digit]);
  ]))

let is_valid_json_number s = Re.execp json_number_re s

let of_string_opt s =
  if not (is_valid_json_number s) then None
  else
    let float_opt =
      match float_of_string_opt s with
      | Some f when not (Float.is_nan f) && not (Float.is_infinite f) -> Some f
      | _ -> None
    in
    let int_opt = int_of_string_opt s in
    Some { int = int_opt; float = float_opt; literal = Some s }

let of_int64 (i : int64) : t =
  (* Check whether i fits in a native int via a roundtrip. *)
  let n = Int64.to_int i in
  if Int64.of_int n = i then of_int n
  else
    (* The value doesn't fit in a native OCaml int (possible on 32-bit
       platforms for large integers).  Use the decimal string; this always
       produces a valid JSON number literal. *)
    match of_string_opt (Int64.to_string i) with
    | Some n -> n
    | None   ->
        (* Int64.to_string is always a valid decimal — this cannot happen. *)
        assert false
