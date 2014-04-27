(*
   Topological sort that doesn't give up on cycles.
*)

module type Param =
sig
  type t
  type id
  val id : t -> id
end

module Make (P : Param) =
struct
  type id = P.id

  type node_state = Visited | Unvisited

  (* graph node with mutable labels *)
  type node = {
    id: P.id;
    value: P.t;
    mutable state: node_state;
  }

  (* all edges of the original graph *)
  type graph = {
    forward: (id, node list) Hashtbl.t;
    backward: (id, node list) Hashtbl.t;
  }

  (* subset of nodes on which iteration and set operations are possible
     (intersection, union, etc.) *)
  module S = Set.Make (
    struct
      type t = node
      let compare a b = Pervasives.compare a.id b.id
    end
  )

  (*
     Algorithm outline:

     Input: directed graph
     Output: a list of node groups sorted topologically, i.e.
             for any group A coming after group B and any node n_a in A
             and any node n_b in B, there is no edge
             going from n_b to n_a.
             ... such that the number of groups is maximum.

     Initialization:
     Build graph structure such that allows following edges both forward
     and backward.

     1. root and leaf elimination: a leaf is a node without outgoing edges,
        a root is a node without incoming edges.
     2. partitioning into strict ancestors (left), cycle (middle),
        and strict descendants (right), and other (independent):
        pick an processed node V (our pivot), determine the set of
        descendant nodes and the set of ancestor nodes by following edges
        from V respectively forward and backward.
        Nodes that belong both to the descendant set
        and to the ancestor set form a cycle with V and are removed
        from the graph.
        Strict ancestors are sorted starting from step 1, strict descendants
        are sorted starting from step 1.
  *)

  let get_neighbors v edges =
    try Hashtbl.find edges v.id
    with Not_found -> []

  let filtered_neighbors v edges graph_nodes =
    let all = get_neighbors v edges in
    List.filter
      (fun neighbor -> S.mem neighbor graph_nodes)
      all

  let pick_one nodes =
    try
      let v = S.choose nodes in
      Some (v, S.remove v nodes)
    with Not_found ->
      None

  let remove_list set l =
    List.fold_left (fun set v -> S.remove v set) set l

  let add_list set l =
    List.fold_left (fun set v -> S.add v set) set l

  let is_root back_edges graph_nodes v =
    filtered_neighbors v back_edges graph_nodes = []

  let eliminate_roots_recursively edges back_edges nodes =
    let rec aux sorted graph_nodes input_nodes =
      match pick_one input_nodes with
      | None ->
          List.rev_map (fun v -> false, S.singleton v) sorted, graph_nodes
      | Some (v, input_nodes) ->
          if is_root back_edges graph_nodes v then
            let sorted = v :: sorted in
            let children = filtered_neighbors v edges graph_nodes in
            let graph_nodes = S.remove v graph_nodes in
            let input_nodes = add_list input_nodes children in
            assert (not (S.mem v input_nodes));
            aux sorted graph_nodes input_nodes
          else
            aux sorted graph_nodes input_nodes
    in
    aux [] nodes nodes

  let eliminate_roots graph nodes =
    eliminate_roots_recursively graph.forward graph.backward nodes

  let eliminate_leaves graph nodes =
    let sorted_leaves, remaining_nodes =
      eliminate_roots_recursively graph.backward graph.forward nodes
    in
    remaining_nodes, List.rev sorted_leaves

  let clear nodes =
    S.iter (fun v -> v.state <- Unvisited) nodes

  let visit edges start_node nodes =
    let rec color acc v =
      match v.state with
      | Visited -> acc
      | Unvisited ->
          v.state <- Visited;
          let acc = S.add v acc in
          List.fold_left (fun acc neighbor ->
            if S.mem neighbor nodes then
              color acc neighbor
            else
              acc
          ) acc (get_neighbors v edges)
    in
    let visited = color S.empty start_node in
    clear visited;
    visited

  let find_descendants graph pivot nodes =
    visit graph.forward pivot nodes

  let find_ancestors graph pivot nodes =
    visit graph.backward pivot nodes

  let rec sort_subgraph graph nodes =
    let sorted_left, nodes = eliminate_roots graph nodes in
    let nodes, sorted_right = eliminate_leaves graph nodes in
    let sorted_middle =
      match pick_one nodes with
      | None -> []
      | Some (pivot, _) -> partition graph pivot nodes
    in
    sorted_left @ sorted_middle @ sorted_right

  and partition graph pivot nodes =
    let ( - ) = S.diff in
    let ancestors = find_ancestors graph pivot nodes in
    let descendants = find_descendants graph pivot nodes in
    let strict_ancestors = ancestors - descendants in
    let strict_descendants = descendants - ancestors in
    let cycle = S.inter descendants ancestors in
    let other = nodes - ancestors - descendants in
    sort_subgraph graph strict_ancestors
    @ [true, cycle]
    @ sort_subgraph graph strict_descendants
    @ sort_subgraph graph other (* could as well be inserted anywhere *)

  (* Data preparation and cleanup *)
  let sort l =
    let node_tbl = Hashtbl.create (2 * List.length l) in
    let make_node x =
      let id = P.id x in
      try Hashtbl.find node_tbl id
      with Not_found ->
        let v = {
          id;
          state = Unvisited;
          value = x;
        } in
        Hashtbl.add node_tbl id v;
        v
    in
    let make_edge edges v1 v2 =
      let l =
        try Hashtbl.find edges v1.id
        with Not_found -> []
      in
      Hashtbl.replace edges v1.id (v2 :: l)
    in
    let forward = Hashtbl.create (2 * List.length l) in
    let backward = Hashtbl.create (2 * List.length l) in
    List.iter (fun (x1, l) ->
      let v1 = make_node x1 in
      List.iter (fun x2 ->
        let v2 = make_node x2 in
        make_edge forward v1 v2;
        make_edge backward v2 v1;
      ) l
    ) l;
    let graph = { forward; backward } in
    let nodes = Hashtbl.fold (fun k v set -> S.add v set) node_tbl S.empty in

    let sorted_groups = sort_subgraph graph nodes in

    (* Export as lists *)
    List.map (fun (is_cyclic, set) ->
      is_cyclic, List.map (fun node -> node.value) (S.elements set)
    ) sorted_groups
end


(* Testing *)

module Sorter = Make (
struct
  type t = int
  type id = int
  let id x = x
end
)

let rec in_order result a b =
  match result with
  | [] -> false
  | (cyclic, l) :: ll ->
      if List.mem b l then
        false
      else if List.mem a l then
        List.exists (fun (_, l) -> List.mem b l) ll
      else
        in_order ll a b

let rec in_same_cycle result a b =
  match result with
  | [] -> false
  | (cyclic, l) :: ll ->
      cyclic && List.mem a l && List.mem b l
      || in_same_cycle ll a b

let not_in_cycle result x =
  List.exists (function
    | (false, [y]) when y = x -> true
    | _ -> false
  ) result


let seq result a b =
  in_order result a b
  && not (in_order result b a)
  && not (in_same_cycle result a b)

let cyc result a b =
  in_same_cycle result a b
  && not (in_order result a b)
  && not (in_order result b a)

let sng result x =
  not_in_cycle result x

let run_test () =
  Sorter.sort [
    1, [ 2; 3 ];
    2, [ 3 ];
    3, [ 3; 4 ];
    4, [ 3; ];
    5, [ 6 ];
    6, [ 6; 1 ];
    5, [ 7 ];
    7, [ 8 ];
    8, [ 9 ];
    10, [ 10 ];
    11, [ 12 ];
    12, [ 13 ];
    13, [ 11 ];
  ]

let test () =
  let r = run_test () in
  assert (seq r 1 2);
  assert (seq r 1 4);
  assert (seq r 1 3);
  assert (seq r 2 3);
  assert (cyc r 3 4);
  assert (sng r 5);
  assert (seq r 6 1);
  assert (sng r 7);
  assert (sng r 8);
  assert (sng r 9);
  assert (seq r 5 9);
  assert (cyc r 10 10);
  assert (cyc r 11 12);
  assert (cyc r 12 13);
  assert (cyc r 11 13)
