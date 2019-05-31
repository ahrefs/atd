(* Runtime library for WWW-form *)

open Printf

type state = {
  top : bool;
  prefix : string list;
}

type acc = (string list * string) list

type 'a write = state -> acc -> 'a -> acc

exception Error of string

let error s = raise (Error s)

let make_item { top = _; prefix; } = { top = false; prefix = "[]" :: prefix; }

let make_field = function
  | { top = true; prefix; } -> (fun key -> { top = false; prefix = key :: prefix; })
  | { top = false; prefix; } -> (fun key -> { top = false; prefix = "]" :: key :: "[" :: prefix; })

let write_list write_item state acc l =
  List.fold_left (write_item (make_item state)) acc l

let write_array write_item state acc l =
  Array.fold_left (write_item (make_item state)) acc l

let write_string write s =
  match write { top = true; prefix = []; } [] s with
  | [ [], key ] -> key
  | _ -> error "wrong write_string output"

let write_assoc_list write_key write_item state acc l =
  let field = make_field state in
  List.fold_left begin fun acc (key, value) ->
    let key = write_string write_key key in
    write_item (field key) acc value
  end acc l

let write_assoc_array write_key write_item state acc l =
  let field = make_field state in
  Array.fold_left begin fun acc (key, value) ->
    let key = write_string write_key key in
    write_item (field key) acc value
  end acc l

let write_null _state acc () = acc

let write_option write_item state acc x =
  match x with
  | Some x -> write_item state acc x
  | None -> acc

let write_nullable = write_option

let write_string state acc x = (state.prefix, x) :: acc

let write_bool state acc x = write_string state acc (string_of_bool x)

let write_int state acc x = write_string state acc (string_of_int x)

let write_int8 state acc x = write_int state acc (int_of_char x)

let write_int32 state acc x = write_string state acc (Int32.to_string x)

let write_int64 state acc x = write_string state acc (Int64.to_string x)

let write_float state acc x = write_string state acc (string_of_float x)

let write_float_as_int state acc x =
  if x >= min_float && x <= max_float then
    write_int state acc (int_of_float (if x < 0. then x -. 0.5 else x +. 0.5))
  else
    match classify_float x with
    | FP_normal
    | FP_subnormal
    | FP_zero -> write_string state acc (sprintf "%.0f" x)
    | FP_infinite -> error "Cannot convert inf or -inf into a WWW-form int"
    | FP_nan -> error "Cannot convert NaN into a WWW-form int"

let write_float_prec prec state acc x =
  match classify_float x with
  | FP_normal
  | FP_subnormal
  | FP_zero -> write_string state acc (sprintf "%.*f" prec x)
  | FP_infinite -> error "Cannot convert inf or -inf into a WWW-form float"
  | FP_nan -> error "Cannot convert NaN into a WWW-form float"

let www_form_of_acc write x =
  let acc = write { top = true; prefix = []; } [] x in
  List.rev_map (fun (key, value) -> String.concat "" (List.rev key), value) acc
