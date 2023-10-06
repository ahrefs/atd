(*
   Compare two ATD ASTs for incompatibilities

   Specifically, we're focused on incompatibilities in JSON data and we
   honor <json ...> annotations that affect the shape of JSON data.

   1. Identify root types.

   A root type is type defined in the ATD file that is not referenced by
   other type definitions. A root type whose name changes is considered
   possibly problematic while a non-root type can change its name as long
   as its uses remain the same or compatible as before.

   Removed and added root type names are reported. Changes in type parameters
   are not reported and not checked for compatibility at this stage.
   For example, 'type ('a, 'b) t = ('a * 'b) list' can become
   'type ('k, 'v) t = ('k * 'v) list' or even
   'type ('k, 'v) t = ('k * 'v) option' without triggering a warning at this
   stage.

   2. Compare pairs of root types across the older and the newer AST.

   Types are compared structurally i.e. changes in type names don't matter
   as long as they don't affect the JSON representation of data.
   For example, 'int list' and 'nums' are equivalent if 'nums' is defined
   as 'type nums = int list'.

   Comparing two type expressions occurring within a definition requires
   replacing the type names by the type expression they stand for
   (if possible). Instantiating type arguments needs to be done carefully.
   We should report no incompatibility for the following example:

   Old:
     type 'a item = [ Thing of 'a ]
     type 'a items = ('a item * int item)

   New:
     type 'label item = [ Thing of 'label ]
     type thingie = [ Thing of int ]
     type 'a items = ('a item * thingie)

   See the test files for a complete list of expected results for different
   scenarios.
*)

open Printf
open Types
module A = Atd.Ast

module Strings = Set.Make(String)

let get_root_types defs =
  let nonroots =
    List.fold_left (fun nonroots (_loc, (name, _param, _an), expr) ->
      let referenced = A.extract_type_names expr in
      List.fold_right Strings.add referenced nonroots
    ) Strings.empty defs
  in
  (* Exclude built-ins like 'list' without worrying what they are *)
  defs
  |> List.filter_map (fun (_loc, (name, _param, _an), expr) ->
    if Strings.mem name nonroots then None
    else Some name
  )

(* Split root type names into:
   - left-only
   - intersection
   - right-only
*)
let split_root_types root_types1 root_types2 =
  let a = Strings.of_list root_types1 in
  let b = Strings.of_list root_types2 in
  Strings.diff a b, Strings.inter a b, Strings.diff b a

let report_deleted_root_types defs name_set =
  List.filter_map (fun (loc, (name, _param, _an), _expr) ->
    if Strings.mem name name_set then
      Some {
        direction = Backward;
        kind = Deleted_root_type { def_name = name };
        location_old = Some loc;
        location_new = None;
        description =
          sprintf "The definition for type '%s' no longer exists."
            name
      }
    else
      None
  ) defs

let report_added_root_types defs name_set =
  List.filter_map (fun (loc, (name, _param, _an), _expr) ->
    if Strings.mem name name_set then
      Some {
        direction = Forward;
        kind = Deleted_root_type { def_name = name };
        location_old = None;
        location_new = Some loc;
        description = sprintf "There is a new type named '%s'." name
      }
    else
      None
  ) defs

let finding_group (a : finding) =
  match a.location_old, a.location_new with
  | None, None -> (* should probably not exist *) 1
  | Some _, None -> 2
  | None, Some _ -> 3
  | Some _, Some _ -> 4

let compare_opt_location (a : Atd.Loc.t option) (b : Atd.Loc.t option) =
    match a, b with
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0
  | Some a, Some b -> Atd.Loc.compare a b

(* Compare results so as to sort them in the following order:
   1. Exists only in the old file
   2. Exists only in the new file
   3a. Position in the old file
   3b. Position in the new file
*)
let compare_findings (a : finding) (b : finding) =
  (* sort by left-only, right-only, both *)
  let c = Int.compare (finding_group a) (finding_group b) in
  if c <> 0 then c
  else
    (* within a group, sort normally by location *)
    let c = compare_opt_location a.location_old b.location_old in
    if c <> 0 then c
    else
      compare_opt_location a.location_new b.location_new

(*
   Expectations:
   - the ASTs have been monomorphized
   - where a node substitution occurred in the AST, the location was preserved
     so as to point accurately to the source code location.
*)
let asts (ast1 : A.full_module) (ast2 : A.full_module) : result =
  let _head1, body1 = ast1 in
  let _head2, body2 = ast2 in
  let defs1 = body1 |> List.map (function A.Type x -> x) in
  let defs2 = body2 |> List.map (function A.Type x -> x) in
  let root_types1 = get_root_types defs1 in
  let root_types2 = get_root_types defs2 in
  let left_only, shared, right_only =
    split_root_types root_types1 root_types2
  in
  let results =
    [ report_deleted_root_types defs1 left_only;
      report_added_root_types defs2 right_only ]
  in
  List.flatten results
  |> List.sort compare_findings
