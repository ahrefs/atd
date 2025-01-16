(*
   Take a list of type root types and identify the types that can't be reached
   from these roots.

   root = name of a type from which we pull all the type names it depends on.
*)

open Atd

let get_type_name_dependencies
    (module_ : Ast.full_module) : string list * (string -> string list) =
  let (_head, items) = module_ in
  let defs = List.map (function Ast.Type x -> x) items in
  (* dependency table: left-hand type name -> right-hand type names *)
  let deps = Hashtbl.create 100 in
  List.iter (fun (_loc, (name, _param, _annot), expr) ->
    let names = Ast.extract_type_names expr in
    Hashtbl.add deps name names
  ) defs;
  let defined_names =
    List.map (fun (_loc, (name, _param, _annot), expr) -> name) defs
  in
  let get_deps name =
    match Hashtbl.find_opt deps name with
    | None -> []
    | Some xs -> xs
  in
  defined_names, get_deps

(*
   Algorithm:
   1. Visit all type definitions reachable from roots and mark them
   as visited.
   2. Extract the list of type definitions that were not visited. Return
   them.
*)
let check_root_types_superset roots (module_ : Atd.Ast.full_module) =
  let defined_names, get_deps = get_type_name_dependencies module_ in
  let visited = Hashtbl.create 100 in
  let rec visit name =
    if not (Hashtbl.mem visited name) then (
      Hashtbl.add visited name ();
      List.iter visit (get_deps name)
    )
  in
  List.iter visit roots;
  List.filter (fun name -> not (Hashtbl.mem visited name)) defined_names
