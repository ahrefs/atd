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

(* Check that [s] is a syntactically valid JSON number literal.
   JSON grammar: minus? ( 0 | [1-9][0-9]* ) ( '.' [0-9]+ )? ( [eE] [+-]? [0-9]+ )? *)
let is_valid_json_number s =
  let n = String.length s in
  if n = 0 then false
  else
    let i = ref 0 in
    if s.[!i] = '-' then incr i;
    let int_start = !i in
    (* Integer part *)
    (if !i < n && s.[!i] = '0' then incr i
     else while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done);
    if !i = int_start then false (* no digits *)
    else
      let ok = ref true in
      (* Optional fractional part *)
      if !i < n && s.[!i] = '.' then begin
        incr i;
        let frac_start = !i in
        while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
        if !i = frac_start then ok := false
      end;
      (* Optional exponent *)
      if !ok && !i < n && (s.[!i] = 'e' || s.[!i] = 'E') then begin
        incr i;
        if !i < n && (s.[!i] = '+' || s.[!i] = '-') then incr i;
        let exp_start = !i in
        while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
        if !i = exp_start then ok := false
      end;
      !ok && !i = n

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
