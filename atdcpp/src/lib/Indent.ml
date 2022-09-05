type node =
  | Line of string
  | Block of node list

type t = node list

let as_string node =
  match node with
    | Line l -> l
    | Block _ -> "Block"

let rec as_strings nodes = 
  match nodes with 
    [] -> ""
    | h::t -> as_string h ^ as_strings t

let write_file path nodes = 
  let oc = open_out path in
  Printf.fprintf oc "%s\n" (as_strings nodes);
  close_out oc;
