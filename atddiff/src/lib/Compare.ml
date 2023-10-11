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

let kind_of_expr (e : A.type_expr) =
  match e with
  | Name _ -> "unresolved type name"
  | Sum _ -> "sum type or enum"
  | Record _ -> "record/object"
  | Tuple _ -> "tuple"
  | List _ -> "list/array"
  | Option _ -> "option"
  | Nullable _ -> "nullable"
  | Shared _ -> "shared"
  | Wrap _ -> "wrap"
  | Tvar _ -> "type variable"

let a noun =
  match noun with
  | "" -> "??"
  | s ->
      match s.[0] with
      | 'a' | 'e' | 'i' | 'o' | 'u' -> "an"
      | _ -> "a"

let a_kind_of_expr e =
  let noun = kind_of_expr e in
  (a noun) ^ " " ^ noun

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
        kind = Deleted_root_type;
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
        kind = Deleted_root_type;
        location_old = None;
        location_new = Some loc;
        description = sprintf "There is a new type named '%s'." name
      }
    else
      None
  ) defs

(* Produce a read-only table of type definitions *)
let make_def_table (defs : A.type_def list) : (string, A.type_def) Hashtbl.t =
  let tbl = Hashtbl.create 100 in
  List.iter (fun ((loc, (name, _param, _an), _expr) as def) ->
    Hashtbl.replace tbl name def
  ) defs;
  tbl

let get_def def_tbl name : A.type_def =
  match Hashtbl.find_opt def_tbl name with
  | None -> invalid_arg ("get_def: " ^ name)
  | Some def -> def

(*
   Resolve the type expression into one that is not a name, if possible.

   The result may only be a name if the name can't be resolved.
   It occurs in the following cases:
   - cyclic type definition
   - predefined name that doesn't use a dedicated constructor (e.g. 'list'
     in some cases (?))
   We could revise this to guarantee that the returned node is never
   a 'Name' if it helps.

   Assumption: all type names are built-in or monomorphic.
*)
let deref_expr def_tbl orig_e =
  let rec deref_expr e =
      match (e : A.type_expr) with
      | Name (_loc, (_loc2, name, _args), _an) ->
          let (_loc, (_name, param, _an), e) = get_def def_tbl name in
          check_deref_expr e
      | Sum _
      | Record _
      | Tuple _
      | List _
      | Option _
      | Nullable _
      | Shared _ as e -> e
      | Wrap _ -> assert false
      | Tvar _ -> assert false
  and check_deref_expr e =
    if e == orig_e then
      (* cycle *)
      e
    else
      deref_expr e
  in
  deref_expr orig_e

let report_structural_mismatches defs1 defs2 shared_root_types =
  let def_tbl1 = make_def_table defs1 in
  let def_tbl2 = make_def_table defs2 in
  let get_expr1 e = deref_expr def_tbl1 e in
  let get_expr2 e = deref_expr def_tbl2 e in
  let findings : (string * finding) list ref = ref [] in
  let cmp_expr def_name e1 e2 =
    let add x =
      findings := (def_name, x) :: !findings in
    let rec cmp_expr e1 e2 : unit =
      match get_expr1 e1, get_expr2 e2 with
      | Sum (loc1, cases1, _an1), Sum (loc2, cases2, _an2) ->
          (* TODO: compare JSON annotations an1 and an2 *)
          cmp_variants loc1 loc2 cases1 cases2
      | Record (loc1, fields1, an1), Record (loc2, fields2, an2) ->
          (* TODO: compare JSON annotations an1 and an2 *)
          cmp_fields loc1 loc2 fields1 fields2
      | Tuple (loc1, cells1, an1), Tuple (loc2, cells2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_tuple_cells loc1 loc2 cells1 cells2
      | List (loc1, e1, an1), List (loc2, e2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_expr e1 e2
      | Option (loc1, e1, an1), Option (loc2, e2, an2)
      | Nullable (loc1, e1, an1), Nullable (loc2, e2, an2)
      | Shared (loc1, e1, an1), Shared (loc2, e2, an2) ->
          (* TODO: compare JSON annotations an1 and an2? *)
          cmp_expr e1 e2
      | Name (_, (loc1, name1, args1), _an1),
        Name (_, (loc2, name2, args2), _an2) ->
          (* TODO: does this happen with 'list', 'option, etc.? *)
          if name1 <> name2 then
            add {
              direction = Both;
              kind = Incompatible_type;
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Type names '%s' and '%s' are not the same and may not \
                         be compatible."
                  name1 name2;
            }
          else
            ()
      | Wrap _, _ | _, Wrap _ -> assert false
      | Tvar _, _ | Tvar _, _ -> assert false
      | (Sum _
        | Record _
        | Tuple _
        | List _
        | Option _
        | Nullable _
        | Shared _
        | Name _) as e1, e2 ->
          add {
            direction = Both;
            kind = Incompatible_type;
            location_old = Some (A.loc_of_type_expr e1);
            location_new = Some (A.loc_of_type_expr e2);
            description =
              sprintf "Incompatible kinds of types: %s is now %s."
                (kind_of_expr e1) (a_kind_of_expr e2);
          }
    and cmp_variants loc1 loc2 cases1 cases2 =
      let with_names cases =
        (* TODO: apply annotations *)
        cases
        |> List.map (fun (x : A.variant) ->
          match x with
          | Variant (loc, (name, an), opt_e) -> name, (loc, (name, an), opt_e)
          | Inherit _ -> assert false
        )
      in
      let named1 = with_names cases1 in
      let named2 = with_names cases2 in
      let names1 = List.map fst named1 |> Strings.of_list in
      let names2 = List.map fst named2 |> Strings.of_list in
      let left_only = Strings.diff names1 names2 |> Strings.elements in
      let shared = Strings.inter names1 names2 |> Strings.elements in
      let right_only = Strings.diff names2 names1 |> Strings.elements in
      left_only
      |> List.iter (fun name ->
        let loc, (_name, _an), _opt_e = List.assoc name named1 in
        add {
          direction = Backward;
          kind = Missing_variant { variant_name = name };
          location_old = Some loc;
          location_new = None;
          description = sprintf "Case '%s' disappeared." name
        }
      );
      right_only
      |> List.iter (fun name ->
        let loc, (_name, _an), _opt_e = List.assoc name named2 in
        add {
          direction = Backward;
          kind = Missing_variant { variant_name = name };
          location_old = None;
          location_new = Some loc;
          description = sprintf "Case '%s' is new." name
        }
      );
      shared
      |> List.iter (fun name ->
        let loc1, _, e1 = List.assoc name named1 in
        let loc2, _, e2 = List.assoc name named2 in
        match e1, e2 with
        | None, None -> ()
        | Some e1, Some e2 ->
            cmp_expr e1 e2
        | Some _, None ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' no longer has an argument." name
            }
        | None, Some _ ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' used to not have an argument." name
            }
      )
    and cmp_fields loc1 loc2 fields1 fields2 =
      let with_names fields =
        (* TODO: apply annotations *)
        fields
        |> List.map (fun (field : A.field) ->
          match field with
          | `Field ((_, (name, _, _), _) as x) -> name, x
          | `Inherit _ -> assert false
        )
      in
      let named1 = with_names fields1 in
      let named2 = with_names fields2 in
      let names1 = List.map fst named1 |> Strings.of_list in
      let names2 = List.map fst named2 |> Strings.of_list in
      let left_only = Strings.diff names1 names2 |> Strings.elements in
      let shared = Strings.inter names1 names2 |> Strings.elements in
      let right_only = Strings.diff names2 names1 |> Strings.elements in
      left_only
      |> List.iter (fun name ->
        let loc, (_name, kind, _an), _e = List.assoc name named1 in
        add {
          direction = Forward;
          kind = Missing_field { field_name = name };
          location_old = Some loc;
          location_new = None;
          description = sprintf "Field '%s' disappeared." name
        }
      );
      right_only
      |> List.iter (fun name ->
        let loc, (_name, kind, _an), _opt_e = List.assoc name named2 in
        add {
          direction = Backward;
          kind = Missing_field { field_name = name };
          location_old = None;
          location_new = Some loc;
          description = sprintf "Field '%s' is new." name
        }
      );
      shared
      |> List.iter (fun name ->
        let loc1, _, e1 = List.assoc name named1 in
        let loc2, _, e2 = List.assoc name named2 in
        match e1, e2 with
        | None, None -> ()
        | Some e1, Some e2 ->
            cmp_expr e1 e2
        | Some _, None ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' no longer has an argument." name
            }
        | None, Some _ ->
            add {
              direction = Both;
              kind = Missing_variant_argument { variant_name = name };
              location_old = Some loc1;
              location_new = Some loc2;
              description =
                sprintf "Case '%s' used to not have an argument." name
            }
      )


    and cmp_tuple_cells loc1 loc2 cells1 cells2 =
      let n1 = List.length cells1 in
      let n2 = List.length cells2 in
      if n1 <> n2 then
        add {
          direction = Both;
          kind = Incompatible_type;
          location_old = Some (A.loc_of_type_expr e1);
          location_new = Some (A.loc_of_type_expr e2);
          description = sprintf "Incompatible tuple lengths";
        }
      else
        List.iter2 (fun (_loc1, e1, _an1) (_loc2, e2, _an2) ->
          (* TODO: honor annotations? *)
          cmp_expr e1 e2
        ) cells1 cells2
    in
    cmp_expr e1 e2
  in
  List.iter (fun name ->
    let (loc1, (_name, param, _an), e1) = get_def def_tbl1 name in
    let (loc2, (_name, param, _an), e2) = get_def def_tbl2 name in
    cmp_expr name e1 e2
  ) shared_root_types;
  !findings

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
   4. Everything else (expected to not occur)
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
      let c = compare_opt_location a.location_new b.location_new in
      if c <> 0 then c
      else Stdlib.compare a b

let group_and_sort_findings xs =
  let tbl = Hashtbl.create 100 in
  xs
  |> List.iter (fun (name, finding) ->
    match Hashtbl.find_opt tbl finding with
    | None ->
        Hashtbl.add tbl finding (ref [name])
    | Some names ->
        names := name :: !names
  );
  Hashtbl.fold (fun finding names acc -> (finding, !names) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> compare_findings a b)

(*
   Expectations:
   - the ASTs have been monomorphized. Any polymorphic type definition
     was removed. The only remaining parametrized types are the builtins
     'list', 'option', etc.
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
  let shared, findings =
    (*
       We don't handle these because it would be complicated.
       Exposing unused parametrized types in an ATD interface is
       a bad practice for a definitive interface. It will become reasonable
       practice once ATD files can be split into modules.
       Here, we emit a warning that we're not handling this case.
    *)
    report_parametrized_root_types defs1 defs2 (Strings.elements shared)
  in
  let findings = [
    report_deleted_root_types defs1 left_only;
    report_added_root_types defs2 right_only;
  ] in
  let findings =
    report_structural_mismatches defs1 defs2 (Strings.elements shared)
  in
  findings
  |> List.flatten
  |> group_and_sort_findings
