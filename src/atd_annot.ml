

open Printf

type t = Atd_ast.annot

let error_at loc s =
  failwith (sprintf "%s:\n%s" (Atd_ast.string_of_loc loc) s)

let has_section k l =
  try ignore (List.assoc k l); true
  with Not_found -> false

let has_field k k2 l =
  List.exists (
    fun k1 ->
      try
        (* each section must be unique *)
        let _, l2 = List.assoc k1 l in
        ignore (List.assoc k2 l2);
        true
      with Not_found -> false
  ) k

let rec find f = function
    [] -> None
  | x :: l ->
      match f x with
          None -> find f l
        | Some _ as y -> y

let get_flag k k2 l =
  let result =
    find (
      fun k1 ->
        try
          (* each section must be unique *)
          let loc, l2 = List.assoc k1 l in
          let loc, o = List.assoc k2 l2 in
          match o with
              None -> Some true
            | Some "true" -> Some true
            | Some "false" -> Some false
            | Some s ->
                error_at loc
                  (sprintf "Invalid value %S for flag %s.%s" s k1 k2)
        with Not_found -> None
    ) k
  in
  match result with
      None -> false
    | Some x -> x

let get_field parse default k k2 l =
  let result =
    find (
      fun k1 ->
        try
          (* each section must be unique *)
          let loc, l2 = List.assoc k1 l in
          let loc, o = List.assoc k2 l2 in
          match o with
              Some s ->
                (match parse s with
                     Some x as y -> y
                   | None ->
                       error_at loc
                         (sprintf "Invalid annotation <%s %s=%S>" k1 k2 s)
                )
            | None ->
                error_at loc
                  (sprintf "Missing value for annotation %s.%s" k1 k2)
        with Not_found ->
          None
    ) k
  in
  match result with
      None -> default
    | Some x -> x

(* replace first occurrence, if any *)
let rec replace k v = function
    (k', _) as x :: l ->
      if k = k' then
        (k, v) :: l
      else
        x :: replace k v l
  | [] ->
      []

let set_field loc k k2 v l : Atd_ast.annot =
  try
    let section_loc, section = List.assoc k l in
    let section =
      try
        let _field = List.assoc k2 section in
        replace k2 (loc, v) section
      with Not_found ->
        (k2, (loc, v)) :: section
    in
    replace k (section_loc, section) l

  with Not_found ->
    (k, (loc, [ k2, (loc, v) ])) :: l


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

let override_values x1 x2 = x1

let override_fields (loc1, l1) (loc2, l2) =
  (loc1, collapse override_values (l1 @ l2))

let merge l =
  collapse override_fields l

let create_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    if !n < 0 then
      failwith "Atd_annot.create_id: counter overflow"
    else
      string_of_int !n
