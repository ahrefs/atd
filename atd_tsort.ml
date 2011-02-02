(* $Id: atd_tsort.ml 38554 2010-03-18 23:54:35Z martin $ *)

open Printf

type ('a, 'b) node = ('a * 'a list * 'b)

module type Ordered =
sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (Param : Ordered) :
sig
  val sort : (Param.t, 'a) node list -> (bool * 'a list) list
end =
struct

  module S = Set.Make (Param)
  module M = Map.Make (Param)

  type state = White | Grey | Black

  let fst3 (x, _, _) = x

  let init_states l =
    List.fold_left (fun m x -> M.add (fst3 x) (ref White) m) M.empty l

  let get_state key states =
    try !(M.find key states)
    with Not_found ->
      invalid_arg (sprintf "Atd_tsort: undefined child node %s"
		     (Param.to_string key))

  let set_state key state states =
    try M.find key states := state
    with Not_found ->
      invalid_arg (sprintf "Atd_tsort: undefined child node %s"
		     (Param.to_string key))

  let merge (s1, l1, ll1) (s2, l2, ll2) =
    (S.union s1 s2, l1 @ l2, ll1 @ ll2)

  let map_of_list l = 
    List.fold_left (fun m x -> M.add (fst3 x) x m) M.empty l

  let get_node key graph =
    try M.find key graph
    with Not_found ->
      invalid_arg
	(sprintf "Atd_tsort: undefined child node %s" (Param.to_string key))

  let rec sort_root graph states (x : (_, _) node) =
    let key, children, value = x in
    match get_state key states with
	Black -> (S.empty, [], [])
      | Grey -> (S.singleton key, [], [])
      | White ->
	  set_state key Grey states;
	  let closing_nodes, cycle_nodes, sorted =
	    sort_list graph states children in
	  set_state key Black states;
	  if S.is_empty closing_nodes then
	    (closing_nodes, [], (false, [value]) :: sorted)
	  else
	    let closing_nodes = S.remove key closing_nodes in
	    let cycle_nodes = value :: cycle_nodes in
	    if S.is_empty closing_nodes then
	      (closing_nodes, [], (true, cycle_nodes) :: sorted)
	    else
	      (closing_nodes, cycle_nodes, sorted)
	
  and sort_list graph states l =
    List.fold_left (
      fun accu key ->
	merge (sort_root graph states (get_node key graph)) accu
    ) (S.empty, [], []) l
    
  and sort (l : (Param.t, 'a) node list) =
    let graph = map_of_list l in
    let states = init_states l in
    let _, _, sorted =
      sort_list graph states (List.map fst3 l) in
    sorted

end

(* Testing *)
(*
module Test = Make (String)


let test_result =
  Test.sort [
    "1", [ "2"; "3" ], "1";
    "2", [ "3" ], "2";
    "3", [ "3"; "4" ], "3";
    "4", [ "3"; ], "4";
    "5", [ "6" ], "5";
    "6", [ "6"; "1" ], "6";
  ]
*)
