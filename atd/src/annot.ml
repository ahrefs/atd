(*
   Utilities for interpreting annotations of type Ast.annot.
*)

open Stdlib_extra

type t = Ast.annot

let error_at loc s =
  failwith (sprintf "%s:\n%s" (Ast.string_of_loc loc) s)

let fields ~section ~field l =
  List.filter_map (fun (s, (_, fs)) ->
    if s = section then Some fs else None) l
    |> List.map (fun fs ->
      List.filter_map (fun (f, (l, s)) ->
        if f = field then Some (l, s) else None)
      fs)
    |> List.flatten

let field ~section ~field l =
  match fields ~section ~field l with
  | [fieldmatch] -> Some fieldmatch
  | (loc, _) :: others -> error_at loc
    (sprintf "Duplicate annotation %s.%s (also in:\n  %s\n)" section field
    (List.map (fun (loc, _) -> (Ast.string_of_loc loc)) others
     |> String.concat ",\n  "))
  | _ -> None

let has_section k l =
  Option.is_some (List.assoc k l)

let has_field ~sections:k ~field:k2 l =
  List.exists (fun k1 ->
    field ~section:k1 ~field:k2 l
    |> Option.is_some
  ) k

let get_flag ~sections:k ~field:k2 l =
  k
  |> List.find_map (fun k1 ->
    field ~section:k1 ~field:k2 l
    |> Option.map (fun (loc, o) ->
      match o with
      | None | Some "true" -> true
      | Some "false" -> false
      | Some s ->
          error_at loc
            (sprintf "Invalid value %S for flag %s.%s" s k1 k2)))
  |> Option.value ~default:false

let get_field ~parse ~default ~sections:k ~field:k2 l =
  k
  |> List.find_map (fun k1 ->
    let open Option.O in
    field l ~section:k1 ~field:k2 >>= fun (loc, o) ->
    match o with
    | Some s ->
        (match parse s with
           Some _ as y -> y
         | None ->
             error_at loc
               (sprintf "Invalid annotation <%s %s=%S>" k1 k2 s))
    | None ->
        error_at loc
          (sprintf "Missing value for annotation %s.%s" k1 k2))
  |> Option.value ~default

let get_fields ~parse ~sections ~field l =
  List.find_map (fun section ->
    Some (
      fields l ~section ~field
      |> List.map (fun (loc, o) ->
        match o with
        | None ->
            error_at loc
              (sprintf "Missing value for annotation %s.%s" section field)
        | Some s ->
            (match parse s with
             | None ->
                 error_at loc
                   (sprintf "Invalid annotation <%s %s=%S>" section field s)
             | Some v -> v))
    )) sections
  |> Option.value ~default:[]

let get_opt_field ~parse ~sections ~field l =
  let parse s =
    match parse s with
    | None -> None (* indicates parse error *)
    | Some v -> Some (Some v)
  in
  get_field ~parse ~default:None ~sections ~field l

let set_field ~loc ~section:k ~field:k2 v l : Ast.annot =
  match List.assoc k l with
  | None -> (k, (loc, [ k2, (loc, v) ])) :: l
  | Some (section_loc, section) ->
      let section_loc, section = List.assoc_exn k l in
      let section =
        match List.assoc k2 section with
        | None -> (k2, (loc, v)) :: section
        | Some _ -> List.assoc_update k2 (loc, v) section
      in
      List.assoc_update k (section_loc, section) l

let get_loc ~sections:k ~field:k2 l =
  k
  |> List.find_map (fun k1 ->
    let open Option.O in
    field l ~section:k1 ~field:k2 >>= fun (loc, _o) -> Some loc)

let get_loc_exn ~sections ~field l =
  get_loc ~sections ~field l |> Option.value_exn

let collapse merge l =
  let tbl = Hashtbl.create 10 in
  let n = ref 0 in

  List.iter (
    fun (s1, f1) ->
      incr n;
      try
        let _, f2 = Hashtbl.find tbl s1 in
        Hashtbl.replace tbl s1 (!n, merge f1 f2)
      with Not_found ->
        Hashtbl.add tbl s1 (!n, f1)
  ) (List.rev l);

  let l = Hashtbl.fold (fun s (i, f) l -> (i, (s, f)) :: l) tbl [] in
  let l = List.sort (fun (i, _) (j, _) -> compare j i) l in
  List.map snd l

let override_values x1 _ = x1

let override_fields (loc1, l1) (_, l2) =
  (loc1, collapse override_values (l1 @ l2))

let merge l =
  collapse override_fields l

let create_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    if !n < 0 then
      failwith "Annot.create_id: counter overflow"
    else
      string_of_int !n

type node_kind =
  | Module_head
  | Type_def
  | Type_expr
  | Variant
  | Cell
  | Field

type schema_field = node_kind * string

type schema_section = {
  section: string;
  fields: schema_field list;
}

type schema = schema_section list

let validate_section sec root =
  (* split fields by location where they may occur *)
  let in_module_head = ref [] in
  let in_import = ref [] in
  let in_type_def = ref [] in
  let in_type_expr = ref [] in
  let in_variant = ref [] in
  let in_cell = ref [] in
  let in_field = ref [] in
  sec.fields
  |> List.iter (fun (kind, field_name) ->
    let acc =
      match kind with
      | Module_head -> in_module_head
      | Type_def -> in_type_def
      | Type_expr -> in_type_expr
      | Variant -> in_variant
      | Cell -> in_cell
      | Field -> in_field
    in
    acc := field_name :: ! acc
  );
  let check acc =
    let allowed_fields = List.rev !acc in
    fun _node (an : Ast.annot) () ->
      an
      |> List.iter (fun ((sec_name, (loc, fields)) : Ast.annot_section) ->
        if sec_name = sec.section then
          fields
          |> List.iter (fun (field_name, (loc2, _opt_val)) ->
            if not (List.mem field_name allowed_fields) then
              Ast.error_at loc2
                (sprintf "Invalid or misplaced annotation <%s ... %s... >"
                   sec_name field_name)
          )
      )
  in
  Ast.fold_annot
    ~module_:(check in_module_head)
    ~import:(check in_import)
    ~type_def:(check in_type_def)
    ~type_expr:(check in_type_expr)
    ~variant:(check in_variant)
    ~cell:(check in_cell)
    ~field:(check in_field)
    root ()

let validate schema root =
  List.iter (fun sec ->
    validate_section sec root
  ) schema
