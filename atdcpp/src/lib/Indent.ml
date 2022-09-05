open Printf

type node =
  | Line of string
  | Block of node list
  | Inline of node list

type t = node list

let rec is_empty_node = function
  | Line "" -> true
  | Line _ -> false
  | Block xs -> List.for_all is_empty_node xs
  | Inline xs -> List.for_all is_empty_node xs
(* let as_string node = *)
  (* match node with *)
    (* | Line l -> Printf.sprintf "%s\n" l *)
    (* | Block _ ->  *)
    (* | Inline _ -> "Inline" *)

let rec write_to_buffer oc node =
  match node with 
  | Line l -> fprintf oc "%s\n" l
  | Block b -> List.iter (write_to_buffer oc) b
  | Inline b -> List.iter (write_to_buffer oc) b

let write_file path nodes = 
  let oc = open_out path in
  List.iter (write_to_buffer oc) nodes;
  close_out oc;
