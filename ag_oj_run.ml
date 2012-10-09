(*
  Runtime library for JSON
*)

open Printf

exception Error of string

(*
  Error messages
*)
let error s = raise (Error s)

let list_iter f sep x l =
  let rec aux f sep x = function
      [] -> ()
    | y :: l ->
	sep x;
	f x y;
	aux f sep x l
  in
  match l with
      [] -> ()
    | y :: l ->
	f x y;
	aux f sep x l

let array_iter f sep x a =
  let n = Array.length a in
  if n > 0 then (
    f x (Array.unsafe_get a 0);
    for i = 1 to n - 1 do
      sep x;
      f x (Array.unsafe_get a i)
    done
  )

let write_comma ob =
  Bi_outbuf.add_char ob ','

let write_list write_item ob l =
  Bi_outbuf.add_char ob '[';
  list_iter write_item write_comma ob l;
  Bi_outbuf.add_char ob ']'

let write_array write_item ob a =
  Bi_outbuf.add_char ob '[';
  array_iter write_item write_comma ob a;
  Bi_outbuf.add_char ob ']'

let write_assoc_list write_item ob l =
  Bi_outbuf.add_char ob '{';
  list_iter (
    fun ob (k, v) ->
      Yojson.Safe.write_string ob k;
      Bi_outbuf.add_char ob ':';
      write_item ob v
  ) write_comma ob l;
  Bi_outbuf.add_char ob '}'

let write_assoc_array write_item ob l =
  Bi_outbuf.add_char ob '{';
  array_iter (
    fun ob (k, v) ->
      Yojson.Safe.write_string ob k;
      Bi_outbuf.add_char ob ':';
      write_item ob v
  ) write_comma ob l;
  Bi_outbuf.add_char ob '}'


let write_option write_item ob = function
    None -> Bi_outbuf.add_string ob "<\"None\">"
  | Some x ->
      Bi_outbuf.add_string ob "<\"Some\":";
      write_item ob x;
      Bi_outbuf.add_string ob ">"

let write_std_option write_item ob = function
    None -> Bi_outbuf.add_string ob "\"None\""
  | Some x ->
      Bi_outbuf.add_string ob "[\"Some\",";
      write_item ob x;
      Bi_outbuf.add_string ob "]"

let write_nullable write_item ob = function
    None -> Bi_outbuf.add_string ob "null"
  | Some x -> write_item ob x

let write_int8 ob x =
  Yojson.Safe.write_int ob (int_of_char x)

let write_int32 ob x =
  Bi_outbuf.add_string ob (Int32.to_string x)

let write_int64 ob x =
  Bi_outbuf.add_string ob (Int64.to_string x)

let read_null p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_null p lb

let read_bool p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_bool p lb

let read_int p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_int p lb

let read_int8 p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_int8 p lb

let read_int32 p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_int32 p lb

let read_int64 p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_int64 p lb

let read_number p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_number p lb

let read_string p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_string p lb

let read_list read_item p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_list read_item p lb

let read_array read_item p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_array read_item p lb

let read_assoc_list_rev read_item p lb =
  Yojson.Safe.read_space p lb;
  let read acc k p lb = (k, read_item p lb) :: acc in
  Yojson.Safe.read_fields read [] p lb

let read_assoc_list read_item p lb =
  List.rev (read_assoc_list_rev read_item p lb)

let array_of_rev_list l =
  match l with
      [] -> [| |]
    | x :: tl ->
	let len = List.length l in
	let a = Array.make len x in
	let r = ref tl in
	for i = len - 2 downto 0 do
	  a.(i) <- List.hd !r;
	  r := List.tl !r
	done;
	a

let read_assoc_array read_item p lb =
  array_of_rev_list (read_assoc_list_rev read_item p lb)

let read_until_field_value p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_colon p lb;
  Yojson.Safe.read_space p lb

let missing_tuple_fields len req_fields =
  let missing =
    List.fold_right (
      fun i acc -> if i >= len then i :: acc else acc
    ) req_fields []
  in
  error (sprintf "Missing tuple field%s %s"
	   (if List.length missing > 1 then "s" else "")
	   (String.concat ", " (List.map string_of_int missing)))


let missing_fields bit_fields field_names =
  let acc = ref [] in
  for z = Array.length field_names - 1 downto 0 do
    let i = z / 31 in
    let j = z mod 31 in
    if bit_fields.(i) land (1 lsl j) = 0 then
      acc := field_names.(z) :: !acc
  done;
  error (sprintf "Missing record field%s %s"
	   (if List.length !acc > 1 then "s" else "")
	   (String.concat ", " !acc))

let invalid_variant_tag s =
  error (sprintf "Unsupported variant %S" s)


(* We want an identity function that is not inlined *)
type identity_t = { mutable _identity : 'a. 'a -> 'a }
let identity_ref = { _identity = (fun x -> x) }
let identity x = identity_ref._identity x

(*
  Checking at runtime that our assumptions on unspecified compiler behavior
  still hold.
*)

type t = {
  _a : int option;
  _b : int;
}

let create () =
  { { _a = None; _b = 42 } with _a = None }

let test () =
  let r = create () in
  let v = Some 17 in
  Obj.set_field (Obj.repr r) 0 (Obj.repr v);
  let safe_r = identity r in
  (* r._a is inlined by ocamlopt and equals None
     because the field is supposed to be immutable. *)
  assert (safe_r._a = v)

let () = test ()

(************************************)
